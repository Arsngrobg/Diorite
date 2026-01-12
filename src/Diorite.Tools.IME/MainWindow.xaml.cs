// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    MainWindow.xaml.cs
// Summary: a module for handling GUI functionality and user interactions with elements in the GUI
// Author:  Borngle
// Version: v1.5
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

using System.IO;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;
using Diorite.Lang.API;
using ScottPlot;
using System.Text.Json;
using Diorite.Lang.API.Library;
using Diorite.Lang.API.Syntax.Views;
using IME.Models;
using Microsoft.Win32;
using ScottPlot.Plottables;

namespace IME {
    public partial class MainWindow : Window {
        private bool _updating = false; // Prevents infinite TextChanged recursion
        private string? _currentFile = null; // Stores the path of the currently opened file
        private bool _changed = false; // Tracks if workspace has been changed
        
        // ScottPlot
        private readonly Dictionary<TextBox, IPlottable> _plots = new(); // Maps an input box to a plot
        private readonly Dictionary<TextBox, ScottPlot.Color> _plotColors = new(); // Maps a colour to a plot
        private AxisLimits _previousLimits;
        
        // Runtime
        private readonly DioriteEvaluator _evaluator;
        private Memory _sessionMemory;
        
        public MainWindow() {
            InitializeComponent();
            string basePath = Path.Combine(AppContext.BaseDirectory, "Library", "Stdlib", "base.diorite");
            string trigPath = Path.Combine(AppContext.BaseDirectory, "Library", "Stdlib", "trigonometry.diorite");
            var library = Library.OfFile(basePath);
            library.AddFile(trigPath);
            var baseMemory = library.BuildMemorySnapshot();
            _evaluator = DioriteEvaluator.Configure(baseMemory, _ => {return;});
            _sessionMemory = _evaluator.GetMemory();
            LoadHelp();
            Workspace.Document.TextChanged += WorkspaceTextChanged;
            Loaded += PlotLoaded;
        }
        
        /// <summary>
        /// Initializes the plot after the window has loaded and sets up axis rendering events.
        /// </summary>
        /// <param name="sender"> the object in the XAML that the event was invoked on </param>
        /// <param name="e"> data for the <see cref="RoutedEventArgs"/> </param>
        private void PlotLoaded(object sender, RoutedEventArgs e) {
            plot.Plot.Axes.Bottom.Label.Text = "x";
            plot.Plot.Axes.Left.Label.Text = "y";
            plot.Plot.Legend.IsVisible = false;
            plot.Plot.Legend.Alignment = Alignment.UpperLeft;
            plot.Plot.Axes.AutoScale();
            plot.Refresh();
            plot.Plot.RenderManager.RenderFinished += (o, r) => {
                var limits = plot.Plot.Axes.GetLimits();
                if (_previousLimits == null) {
                    _previousLimits = limits;
                    return;
                }
                if (limits.Left != _previousLimits.Left || limits.Right != _previousLimits.Right ||
                    limits.Top != _previousLimits.Top || limits.Bottom != _previousLimits.Bottom) {
                    PlotAxesChanged(limits);
                    _previousLimits = limits;
                }
            };
            AddInput();
        }

        /// <summary>
        /// Updates all input plots when axes limits change.
        /// </summary>
        /// <param name="limits"> the new axes limits </param>
        private async void PlotAxesChanged(AxisLimits limits) {
            double xRange = limits.Right - limits.Left;
            double yRange = limits.Top - limits.Bottom;
            if (xRange <= 0 || yRange <= 0) {
                return; 
            }
            int maxPoints = 1000;
            int sizeX = Math.Min(maxPoints, Math.Max((int) Math.Ceiling(xRange * 10), 2)); //
            var inputFunctions = new List<(TextBox TextBox, Function? Function)>();
            foreach (var textBox in _plots.Keys.ToList()) {
                string input = textBox.Text;
                var function = Function.OfString(input);
                inputFunctions.Add((textBox, function));
            }
            await Task.Run(() => {
                foreach (var (textBox, function) in inputFunctions) {
                    if (function == null) {
                        continue;
                    }
                    double[] xValues = new double[sizeX];
                    for (int i = 0; i < sizeX; i++) {
                        xValues[i] = limits.Left + i * xRange / (sizeX - 1);
                    }
                    double[] yValues = new double[sizeX];
                    Parallel.For(0, sizeX, i => {
                        var result = _evaluator.EvaluateFunction(function, Value.OfNumber(xValues[i]));
                        yValues[i] = result.Re();
                    });
                    Application.Current.Dispatcher.Invoke(() => {
                        plot.Refresh();
                        if (IsValidPlot(xValues, yValues)) {
                            UpdateInputPlot(textBox, xValues, yValues);
                        }
                    });
                }
            });
        }
        
        /// <summary>
        /// Adds an input <see cref="TextBox"/> for an expression in the <see cref="InputPanel"/>.
        /// </summary>
        private void AddInput() {
            var textBox = new TextBox {
                FontFamily = new FontFamily("Calibri"), FontSize = 32,
                Height = 64, Padding = new Thickness(12),
                FontStyle = FontStyles.Italic, Margin = new Thickness(0, 0, 0, 12),
                BorderBrush = Brushes.Gray
            };
            textBox.TextChanged += InputTextChanged;
            textBox.TextChanged += TextChanged; // Caret updating
            textBox.PreviewKeyDown += KeyDownHandler;
            textBox.GotFocus += (sender, routedEventArgs) => {
                ProcessInput(textBox); // Output updates again
            };
            InputPanel.Children.Add(textBox);
        }
        
        /// <summary>
        /// Handler for when text changes in an input box.
        /// </summary>
        /// <param name="sender"> the object in the XAML that the event was invoked on </param>
        /// <param name="e"> data for the <see cref="TextChangedEventArgs"/> </param>
        private void InputTextChanged(object sender, TextChangedEventArgs e) {
            TextBox textBox = sender as TextBox;
            ProcessInput(textBox);
        }
        
        /// <summary>
        /// Checks if a set of x and y values are valid for plotting.
        /// </summary>
        /// <param name="xValues"> a range of x values </param>
        /// <param name="yValues"> a range of y values </param>
        /// <returns> true if valid, false if not </returns>
        private bool IsValidPlot(double[] xValues, double[] yValues) {
            if (xValues == null || yValues == null) return false;
            if (xValues.Length == 0 || yValues.Length == 0) return false;
            if (xValues.Any(double.IsNaN) || yValues.Any(double.IsNaN)) return false;
            if (xValues.All(v => v == xValues[0])) return false;
            if (yValues.All(v => v == yValues[0])) return false;
            return true;
        }

        /// <summary>
        /// Updates the plot that corresponds to the given <c>textBox</c> input. Removes plots for
        /// invalid inputs.
        /// </summary>
        /// <param name="xValues"> range of x values plot covers </param>
        /// <param name="yValues"> range of y values plot covers </param>
        /// <param name="textBox"> <see cref="TextBox"/> key </param>
        private void UpdateInputPlot(TextBox textBox, double[] xValues, double[] yValues) {
            RemovePlot(textBox);
            ScottPlot.Color colour;
            Scatter scatter;
            if (!_plotColors.TryGetValue(textBox, out colour)) {
                scatter = plot.Plot.Add.Scatter(xValues, yValues);
                _plotColors[textBox] = scatter.Color;
            }
            else {
                _plotColors[textBox] = colour;
                scatter = plot.Plot.Add.Scatter(xValues, yValues, colour);
            }
            scatter.LineWidth = 2;
            _plots[textBox] = scatter;
            textBox.BorderBrush = new SolidColorBrush(System.Windows.Media.Color.FromArgb(colour.A, colour.R, colour.G, colour.B));
            var limits = plot.Plot.Axes.GetLimits();
            if (limits.XRange.Length != 0 && limits.YRange.Length != 0) {
                plot.Refresh();
            }
        }
        
        /// <summary>
        /// Removes the graph plot and the <see cref="_plots"/> mapping for the given <c>textBox</c>.
        /// </summary>
        /// <param name="textBox"> <see cref="TextBox"/> key </param>
        private void RemovePlot(TextBox textBox) {
            if (_plots.TryGetValue(textBox, out var plottable)) {
                plot.Plot.Remove(plottable); // Removes from the graph
                _plots.Remove(textBox); // Removes from the dictionary
                plot.Refresh();
            }
            textBox.BorderBrush = Brushes.Gray;
        }
        
        /// <summary>
        /// Removes the <c>textBox</c> and its corresponding plot from the <see cref="InputPanel"/>.
        /// </summary>
        /// <param name="textBox"> <see cref="TextBox"/> being removed </param>
        private void RemoveInput(TextBox textBox) {
            RemovePlot(textBox);
            InputPanel.Children.Remove(textBox); // Removes input box from the panel
        }

        private void ProcessInput(TextBox textBox) {
            string input = textBox.Text.Trim();
            if (string.IsNullOrEmpty(input)) {
                RemovePlot(textBox);
                Output.Text = "";
                return;
            }
            try {
                var result = _evaluator.EvaluateSource(input);
                _sessionMemory = _evaluator.GetMemory();
                var function = Function.OfString(input);
                if (function != null) {
                    var (xValues, yValues) = PlotFunction(function);
                    if (IsValidPlot(xValues, yValues)) {
                        UpdateInputPlot(textBox, xValues, yValues);
                    }
                }
                if (result != null && result.Length > 0) {
                    Output.Text = result[0]?.ToString() ?? "";
                } 
                else {
                    Output.Text = "";
                }
            }
            catch (Exception exception) {
                Output.Text = exception.Message;
                RemovePlot(textBox);
            }
        }

        private (double[], double[]) PlotFunction(Function function, int size = 100) {
            double[] xValues = new double[size];
            double[] yValues = new double[size];
            for (int i = 0; i < size; i++) {
                double x = -10 + 20.0 * i / (size - 1);
                xValues[i] = x;
                var result = _evaluator.EvaluateFunction(function, Value.OfNumber(x));
                if (result.Equals(Value.Undefined))
                    continue;
                yValues[i] = result.Re();
            }
            return (xValues, yValues);
        }
        
        /// <summary>
        /// Handler for special key presses.
        /// </summary>
        /// <param name="sender"> the object in the XAML that the event was invoked on </param>
        /// <param name="e"> data for the <see cref="KeyEventArgs"/> </param>
        private void KeyDownHandler(object sender, KeyEventArgs e) {
            TextBox textBox = sender as TextBox;
            int index = InputPanel.Children.IndexOf(textBox);
            switch (e.Key) {
                case Key.Enter:
                    AddInput();
                    TextBox newTextBox = InputPanel.Children[index + 1] as TextBox;
                    newTextBox?.Focus();
                    e.Handled = true;
                    break;

                case Key.Back:
                    if (index > 0 && string.IsNullOrEmpty(textBox.Text) && InputPanel.Children.Count > 1) {
                        RemoveInput(textBox);
                        TextBox previousTextBox = InputPanel.Children[index - 1] as TextBox;
                        previousTextBox?.Focus();
                        e.Handled = true;
                    }
                    break;

                case Key.Up:
                    if (index > 0 && InputPanel.Children[index - 1] is TextBox previous) {
                        previous.Focus();
                        previous.CaretIndex = previous.Text.Length;
                        e.Handled = true;
                    }
                    break;

                case Key.Down:
                    if (index < InputPanel.Children.Count - 1 && InputPanel.Children[index + 1] is TextBox next) {
                        next.Focus();
                        next.CaretIndex = next.Text.Length;
                        e.Handled = true;
                    }
                    break;
            }
        }
        
        /// <summary>
        /// Handler for when text in a box is changed.
        /// </summary>
        /// <param name="sender"> the object in the XAML that the event was invoked on </param>
        /// <param name="e"> data for the <see cref="TextChangedEventArgs"/> </param>
        private void TextChanged(object sender, TextChangedEventArgs e) {
            if (_updating) {
                return;
            }
            _updating = true;
            TextBox textBox = sender as TextBox;
            if (textBox != null) {
                int caret = textBox.CaretIndex;
                textBox.CaretIndex = caret;
            }
            _updating = false;
        }

        /// <summary>
        /// Loads and formats help content from JSON file into a collection of <see cref="TextBlock"/> objects.
        /// </summary>
        private void LoadHelp() {
            string json = File.ReadAllText("Resources/help.json");
            var helpContent = JsonSerializer.Deserialize<HelpContent>(json);
            HelpHeader.Text = helpContent.header;
            foreach (var section in helpContent.sections) {
                TextBlock title = new TextBlock {
                    Text = section.title,
                    FontSize = 20,
                    FontWeight = FontWeights.Bold,
                    Margin = new Thickness(0, 10, 0, 4),
                    FontFamily = new FontFamily("Calibri")
                };
                HelpContentPanel.Children.Add(title);
                foreach (var item in section.items) {
                    TextBlock itemBlock = new TextBlock {
                        Text = "• " + item,
                        FontSize = 16,
                        Margin = new Thickness(20, 2, 0, 2),
                        TextWrapping = TextWrapping.Wrap,
                        FontFamily = new FontFamily("Calibri")
                    };
                    HelpContentPanel.Children.Add(itemBlock);
                }
            }
        }

        /// <summary>
        /// Handler for when the help button is clicked.
        /// </summary>
        /// <param name="sender"> the button object in the XAML that the event was invoked on </param>
        /// <param name="e"> data for the <see cref="RoutedEventArgs"/> </param>
        private void HelpClick(object sender, RoutedEventArgs e) {
            Button help = sender as Button;
            if (help != null) {
                Grid helpGrid = HelpMenu;
                helpGrid.Visibility = Visibility.Visible;
            }
        }

        /// <summary>
        /// Handler for when the close help button is clicked.
        /// </summary>
        /// <param name="sender"> the button object in the XAML that the event was invoked on </param>
        /// <param name="e"> data for the <see cref="RoutedEventArgs"/></param>
        private void CloseHelpClick(object sender, RoutedEventArgs e) {
            Button help = sender as Button;
            if (help != null) {
                Grid helpGrid = HelpMenu;
                helpGrid.Visibility = Visibility.Hidden;
            }
        }

        /// <summary>
        /// Handler for loading <c>.diorite</c> or <c>.txt</c> files in to the workspace.
        /// </summary>
        /// <param name="sender"> the button object in the XAML that the event was invoked on </param>
        /// <param name="e"> data for the <see cref="RoutedEventArgs"/> </param>
        private void LoadFileClick(object sender, RoutedEventArgs e) {
            OpenFileDialog dialog = new OpenFileDialog {
                Title = "Load File",
                Filter = "Diorite Files (*.diorite)|*.diorite|Text Files (*.txt)|*.txt|All Files (*.*)|*.*",
                DefaultExt = ".diorite",
                Multiselect = false
            };
            if (dialog.ShowDialog() == true) {
                try {
                    string text = File.ReadAllText(dialog.FileName);
                    Workspace.Text = text;
                    _changed = false;
                    Workspace.CaretOffset = Workspace.Text.Length;
                    int caret = Workspace.CaretOffset;
                    Workspace.CaretOffset = caret;
                    Workspace.Focus();
                    _currentFile = dialog.FileName;
                    UpdateFileName();
                }
                catch (IOException exception) {
                    MessageBox.Show(
                        $"Failed to load file:\n{exception.Message}",
                        "File IO Error",
                        MessageBoxButton.OK,
                        MessageBoxImage.Error
                    );
                }
            }
        }

        /// <summary>
        /// Handler for saving the workspace into a new <c>.diorite</c> or <c>.txt</c> file.
        /// </summary>
        /// <param name="sender"> the button object in the XAML that the event was invoked on </param>
        /// <param name="e"> data for the <see cref="RoutedEventArgs"/> </param>
        private void SaveAsClick(object sender, RoutedEventArgs routedEventArgs) {
            SaveFileDialog dialog = new SaveFileDialog {
                Title = "Save File",
                Filter = "Diorite Files (*.diorite)|*.diorite|Text Files (*.txt)|*.txt",
                DefaultExt = ".diorite"
            };
            if (dialog.ShowDialog() == true) {
                _currentFile = dialog.FileName;
                File.WriteAllText(_currentFile, Workspace.Text);
                _changed = false;
                UpdateFileName();
            }
        }

        /// <summary>
        /// Handler for saving the workspace into the current or new <c>.diorite</c> or <c>.txt</c> file.
        /// </summary>
        /// <param name="sender"> the button object in the XAML that the event was invoked on </param>
        /// <param name="e"> data for the <see cref="RoutedEventArgs"/> </param>
        private void SaveFileClick(object sender, RoutedEventArgs routedEventArgs) {
            if (_currentFile == null) {
                SaveAsClick(sender, routedEventArgs);
            }
            else {
                File.WriteAllText(_currentFile, Workspace.Text);
                _changed = false;
                UpdateFileName();
            }
        }

        /// <summary>
        /// Simple helper to update the file name above the workspace to the opened file
        /// or to nothing if the workspace is cleared.
        /// </summary>
        private void UpdateFileName() {
            if (_currentFile == null) {
                WorkspaceFileName.Text = "File:";
            }
            else {
                string full = Path.GetFileName(_currentFile);
                if (_changed) {
                    full += "*";
                }
                WorkspaceFileName.Text = $"File: {full}";
            }
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="routedEventArgs"></param>
        private void RunClick(object sender, RoutedEventArgs routedEventArgs) {
            try {
                var results = _evaluator.EvaluateSource(Workspace.Text);
                var output = string.Join("\n", results.Select(result => result.ToString()));
            }
            catch (Exception exception) {
                var error =  exception.Message;
            }
        }

        /// <summary>
        /// Handler for clearing the workspace.
        /// </summary>
        /// <param name="sender"> the button object in the XAML that the event was invoked on </param>
        /// <param name="e"> data for the <see cref="RoutedEventArgs"/> </param>
        private void ClearClick(object sender, RoutedEventArgs e) {
            if (Workspace.Text == "") {
                return;
            }
            if (_currentFile == null || _changed) {
                var result = MessageBox.Show(
                    "Are you sure you want to clear your unsaved work?",
                    "Confirm",
                    MessageBoxButton.YesNo,
                    MessageBoxImage.Warning);
                if (result == MessageBoxResult.No) {
                    return;
                }
            }
            Workspace.Text = string.Empty;
            Workspace.Focus();
            _currentFile = null;
            _changed = false;
            UpdateFileName();
        }
        
        /// <summary>
        /// Updates <c>_changed</c> status if the text within the workspace has changed. This is
        /// to give the user a warning before leaving without saving.
        /// </summary>
        /// <param name="sender"> the object in the XAML that the event was invoked on </param>
        /// <param name="eventArgs"> data for the <see cref="EventArgs"/> </param>
        private void WorkspaceTextChanged(object? sender, EventArgs eventArgs) {
            if (!_updating) {
                _changed = true;
                UpdateFileName();
            }
        }
    }
}