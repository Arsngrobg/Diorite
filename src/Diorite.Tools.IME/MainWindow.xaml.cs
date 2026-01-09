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
// Version: v1.2
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

using System.IO;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;
using Diorite.Lang.API;
using Microsoft.FSharp.Collections;
using ScottPlot;
using System.Text.Json;
using IME.Models;
using Microsoft.Win32;
using ICSharpCode.AvalonEdit;
using ICSharpCode.AvalonEdit.Document;

namespace IME {
    public partial class MainWindow : Window {
        private bool _updating = false; // Prevents infinite TextChanged recursion
        private readonly Dictionary<TextBox, IPlottable> _plots = new(); // Maps an input box to a plot
        private AxisLimits _previousLimits;
        private string? _currentFile = null;
        private bool _changed = false;

        public MainWindow() {
            InitializeComponent();
            LoadHelp();
            Workspace.Document.TextChanged += WorkspaceTextChanged;
            Loaded += PlotLoaded;
        }

        private void PlotLoaded(object sender, RoutedEventArgs e) {
            plot.Plot.Axes.Bottom.Label.Text = "x";
            plot.Plot.Axes.Left.Label.Text = "y";
            plot.Plot.Legend.IsVisible = false;
            plot.Plot.Legend.Alignment = Alignment.UpperLeft;
            plot.Plot.Axes.AutoScale();
            plot.Refresh();
            plot.Plot.RenderManager.RenderStarting += (s, e) =>
            {
                var limits = plot.Plot.Axes.GetLimits();
                if (_previousLimits == null)
                {
                    _previousLimits = limits;
                    return;
                }

                if (limits.Left != _previousLimits.Left || limits.Right != _previousLimits.Right ||
                    limits.Top != _previousLimits.Top || limits.Bottom != _previousLimits.Bottom)
                {
                    PlotAxesChanged(limits);
                    _previousLimits = limits;
                }
            };
            AddInput();
        }

        private void PlotAxesChanged(AxisLimits limits)
        {
            double xRange = limits.Right - limits.Left;
            double yRange = limits.Top - limits.Bottom;
            if (xRange <= 0 || yRange <= 0) {
                return;
            }
            double pointsPerUnit = 10; // Smoothness
            int sizeX = Math.Max((int) Math.Ceiling(xRange * pointsPerUnit), 10);
            int sizeY = Math.Max((int) Math.Ceiling(yRange * pointsPerUnit), 10);
            foreach (var pair in _plots) {
                TextBox textBox = pair.Key;
                var oldPlot = pair.Value;
                //plot.Plot.Remove(oldPlot);
            }
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
            InputPanel.Children.Add(textBox);
        }
        
        /// <summary>
        /// Handler for when text changes in an input box. Removes plots for empty input boxes.
        /// </summary>
        /// <param name="sender"> the object in the XAML that the event was invoked on </param>
        /// <param name="e"> data for the <see cref="TextChangedEventArgs"/> </param>
        private void InputTextChanged(object sender, TextChangedEventArgs e) {
            TextBox textBox = sender as TextBox;
            if (string.IsNullOrWhiteSpace(textBox.Text)) {
                RemovePlot(textBox);
                Output.Text = ""; // Prevents hanging errors from remaining 
                return;
            }
            try {
                UpdateInputPlot(textBox);
            }
            catch {
                RemovePlot(textBox);
            }
        }
        
        /// <summary>
        /// Updates the plot that corresponds to the given <c>textBox</c> input. Removes plots for
        /// invalid inputs.
        /// </summary>
        /// <param name="textBox"> <see cref="TextBox"/> key </param>
        private void UpdateInputPlot(TextBox textBox) {
            (double[] xValues, double[] yValues) = ProcessInput(textBox.Text);
            if (xValues.Length == 0 || yValues.Length == 0) {
                RemovePlot(textBox);
            }
            else {
                var scatter = plot.Plot.Add.Scatter(xValues, yValues);
                scatter.LineWidth = 2;
                _plots[textBox] = scatter;
                var colour = scatter.Color;
                var wpfColour = System.Windows.Media.Color.FromArgb(colour.A, colour.R, colour.G, colour.B);
                textBox.BorderBrush = new SolidColorBrush(wpfColour);
            }
            plot.Plot.Axes.AutoScale();
            plot.Refresh();
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
        private void RemoveInput(TextBox textBox)
        {
            RemovePlot(textBox);
            InputPanel.Children.Remove(textBox); // Removes input box from the panel
        }
        
        /// <summary>
        /// Handles evaluation of <c>input</c> expression and updates output text.
        /// </summary>
        /// <param name="input"> input <see cref="string"/> </param>
        /// <returns> a set of x and y values for which the expression has been applied to </returns>
        private (double[], double[]) ProcessInput(string input)
        {
            double[] xValues = new double[]{};
            double[] yValues = new double[]{};
            var tokens = Lexer.tokenize(input);
            var parsed = Parser.parse(tokens);
            if (parsed.IsError) {
                Output.Text = parsed.ErrorValue.ToString();
            }
            else {
                Output.Text = ""; // Prevents hanging errors from remaining
                var resultValue = parsed.ResultValue as Parser.AST.Begin;
                // Regular functions
                if (resultValue.Item.Length == 1 && resultValue.Item[0] is Parser.AST.FunctionDef) {
                    var fn = resultValue.Item[0] as Parser.AST.FunctionDef;
                    var fnId = Parser.FunctionName.NewVariableName(fn.data.identifier.Item1, fn.data.identifier.Item2);
                    (xValues, yValues) = EvaluateFunction(fn, fnId);
                }
                // Equations of a line
                else if (resultValue.Item.Length == 1 &&
                         Interpreter.lineEquation(resultValue.Item[0]) is Parser.AST.FunctionDef fn) {
                    var fnId = Parser.FunctionName.NewVariableName(fn.data.identifier.Item1, fn.data.identifier.Item2);
                    (xValues, yValues) = EvaluateFunction(fn, fnId);
                }
                // y = or x =
                else if (resultValue.Item.Length == 1 && resultValue.Item[0] is Parser.AST.BinaryOperation binaryOperation 
                                                      && binaryOperation.@operator.IsEquals && 
                                                      binaryOperation.left is Parser.AST.Variable variable) {
                    Interpreter.eval(input);
                    var node = Interpreter.Memory.get(variable.id, 0);
                    if (node is Parser.AST.Number num) {
                        double val = num.Item;
                        if (variable.id == 'y') {
                            xValues = new double[] {-10, 10};
                            yValues = new double[] {val, val};
                        }
                        if (variable.id == 'x') {
                            xValues = new double[] {val, val};
                            yValues = new double[] {-10, 10};
                        }
                    }
                }
                // Simple operations
                else {
                    var evaluatedResult = Interpreter.eval(input);
                    resultValue = evaluatedResult.ResultValue as Parser.AST.Begin;
                    if (evaluatedResult.IsError) {
                        Output.Text = evaluatedResult.ErrorValue.ToString();
                    }
                    if (resultValue.Item[0].IsNumber) {
                        var num = ((Parser.AST.Number) resultValue.Item[0]).Item;
                        if (num == (long) num) {
                            Output.Text = $"{(long) num}";
                        }
                        else {
                            Output.Text = $"{num}";
                        }
                    }
                    else {
                        Output.Text = resultValue.Item[0].ToString();
                    }
                }
            }
            return (xValues, yValues);
        }

        /// <summary>
        /// Evaluates a given function over a range of x and y values.
        /// </summary>
        /// <param name="fn"> the <see cref="Parser.AST.FunctionDef"/> node associated with the function </param>
        /// <param name="fnId"> the <see cref="Parser.FunctionName"/> associated with the function </param>
        /// <param name="size"> the range the function is evaluated over </param>
        /// <returns> a set of x and y values for which the function has been applied to </returns>
        public (double[], double[]) EvaluateFunction(Parser.AST.FunctionDef fn, Parser.FunctionName fnId, int size=11) {
            Interpreter.evalTree(fn);
            double[] xValues = new double[size];
            double[] yValues = new double[size];
            if (fn.data.identifier.Item1 == 'x') {
                // Function of y (x = my + c)
                for (int idx = 0; idx < size; idx++) {
                    var y = -(size - 1) / 2 + idx;
                    yValues[idx] = y;
                }
                for (int idx = 0; idx < yValues.Length; idx++) {
                    double y = yValues[idx];
                    var yAsNode = Parser.AST.NewNumber(y);
                    var caller = Parser.AST.NewFunctionCall(fnId,
                        FSharpList.Create(new ReadOnlySpan<Parser.AST>(yAsNode)));
                    var nodeResult = Interpreter.evalTree(caller);
                    if (nodeResult.IsOk && nodeResult.ResultValue.IsNumber) {
                        xValues[idx] = (nodeResult.ResultValue as Parser.AST.Number).Item;
                    }
                }
            }
            else {
                // Function of x (default for functions or y = mx + c)
                for (int idx = 0; idx < size; idx++) {
                    var x = -(size - 1) / 2 + idx;
                    xValues[idx] = x;
                }
                for (int idx = 0; idx < xValues.Length; idx++) {
                    double x = xValues[idx];
                    var xAsNode = Parser.AST.NewNumber(x);
                    var caller = Parser.AST.NewFunctionCall(fnId,
                        FSharpList.Create(new ReadOnlySpan<Parser.AST>(xAsNode)));
                    var nodeResult = Interpreter.evalTree(caller);
                    if (nodeResult.IsOk && nodeResult.ResultValue.IsNumber) {
                        yValues[idx] = (nodeResult.ResultValue as Parser.AST.Number).Item;
                    }
                }
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
            foreach (var section in helpContent.sections)
            {
                TextBlock title = new TextBlock
                {
                    Text = section.title,
                    FontSize = 20,
                    FontWeight = FontWeights.Bold,
                    Margin = new Thickness(0, 10, 0, 4),
                    FontFamily = new FontFamily("Calibri")
                };
                HelpContentPanel.Children.Add(title);
                foreach (var item in section.items)
                {
                    TextBlock itemBlock = new TextBlock
                    {
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
        private void SaveAsClick(object sender, RoutedEventArgs routedEventArgs)
        {
            SaveFileDialog dialog = new SaveFileDialog
            {
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
        private void SaveFileClick(object sender, RoutedEventArgs routedEventArgs)
        {
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
        private void RunClick(object sender, RoutedEventArgs routedEventArgs)
        {
            string code = Workspace.Text;
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