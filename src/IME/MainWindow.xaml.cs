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
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;

using Diorite.Lang;

namespace IME {
    public partial class MainWindow : Window {
        private bool _updating = false; // Prevents infinite TextChanged recursion
        
        public MainWindow() {
            InitializeComponent();
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
                        var tokens = Lexer.tokenize(input);
                        var value = Parser.eval(tokens);
                        string result = value.Item2.ToString();
                        Output.Text = result;
                    }
                    catch (Exception exception) {
                        Output.Text = exception.Message;
                    }
                    inputBox.Text = "";
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
    }
}