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

using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Text.RegularExpressions;

using Diorite.Lang;
using ScottPlot;

namespace IME {
    public partial class MainWindow : Window {
        private bool _updating = false; // Prevents infinite TextChanged recursion
        
        public MainWindow() {
            InitializeComponent();
            Loaded += PlotLoaded;
        }

        private void PlotLoaded(object sender, RoutedEventArgs e) {
            plot.Plot.Axes.Bottom.Label.Text = "x";
            plot.Plot.Axes.Left.Label.Text = "y";
            plot.Plot.Legend.IsVisible = false;
            plot.Plot.Legend.Alignment = Alignment.UpperLeft;
            plot.Plot.Axes.AutoScale();
            plot.Refresh();
        }

        private void UpdatePlot(double[] xValues, double[] yValues, string expression) {
            plot.Plot.Clear();
            var scatter = plot.Plot.Add.Scatter(xValues, yValues);
            scatter.LegendText = expression;
            plot.Plot.Axes.Title.Label.Text = scatter.LegendText;
            scatter.LineWidth = 2;
            scatter.Color = Colors.Green.WithAlpha(0.8);
            plot.Plot.Axes.AutoScale();
            plot.Refresh();
        }
        
        /// <summary>
        /// Handler for special key presses
        /// </summary>
        /// <param name="sender">the object in the XAML that the event was invoked on</param>
        /// <param name="e">data for the <c>KeyEventArgs</c></param>
        private void KeyDownHandler(object sender, KeyEventArgs e) {
            TextBox inputBox = sender as TextBox;
            if (inputBox != null) {
                if (e.Key == Key.Enter) {
                    e.Handled = true;
                    var input = inputBox.Text;
                    try {
                        ProcessInput(input);
                    }
                    catch (Exception exception) {
                        Output.Text = exception.Message;
                    }
                    inputBox.Text = "";
                }
            }
        }

        /// <summary>
        /// Handles evaluation of <c>input</c> and updates output text
        /// </summary>
        /// <param name="input"> input string </param>
        private void ProcessInput(string input)
        {
            var result = Interpreter.eval(input);
            if (result.IsError) {
                Output.Text = result.ErrorValue.ToString();
            }
            else
            {
                var a = result.ResultValue as Parser.AST.Begin;
                var b = a.Item[0];
                if (b.IsNumber)
                {
                    var num = ((Parser.AST.Number) b).Item;
                    if (num == (long)num)
                    {
                        Output.Text = $"{(long) num}";
                    }
                    else
                    {
                        Output.Text = $"{num}";
                    }
                }
                else
                {
                    Output.Text = b.ToString();
                }
            }
        }

        /// <summary>
        /// Handler for special key releases
        /// </summary>
        /// <param name="sender">the object in the XAML that the event was invoked on</param>
        /// <param name="e">data for the <c>KeyEventArgs</c></param>
        private void KeyUpHandler(object sender, KeyEventArgs e) {
            TextBox textBox = sender as TextBox;
            if (textBox != null) {
            }
        }
        
        /// <summary>
        /// Handler for character typing
        /// </summary>
        /// <param name="sender">the object in the XAML that the event was invoked on</param>
        /// <param name="e">data for the <c>TextCompositionEventArgs</c></param>
        private void TextInputHandler(object sender, TextCompositionEventArgs e) {
            TextBox textBox = sender as TextBox;
            if (textBox != null) {
            }
        }

        /// <summary>
        /// Handler for when text in a box is changed
        /// </summary>
        /// <param name="sender">the object in the XAML that the event was invoked on</param>
        /// <param name="e">data for the <c>TextChangedEventArgs</c></param>
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
        /// Handler for when the help button is clicked
        /// </summary>
        /// <param name="sender">the object in the XAML that the event was invoked on</param>
        /// <param name="e">data for the <c>RoutedEventArgs</c></param>
        private void HelpClick(object sender, RoutedEventArgs e) {
            Button help = sender as Button;
            if (help != null) {
                Grid helpGrid = HelpMenu;
                helpGrid.Visibility = Visibility.Visible;
            }
        }

        /// <summary>
        /// Handler for when the close help button is clicked
        /// </summary>
        /// <param name="sender">the object in the XAML that the event was invoked on</param>
        /// <param name="e">data for the <c>RoutedEventArgs</c></param>
        private void CloseHelpClick(object sender, RoutedEventArgs e) {
            Button help = sender as Button;
            if (help != null) {
                Grid helpGrid = HelpMenu;
                helpGrid.Visibility = Visibility.Hidden;
            }
        }
    }
}