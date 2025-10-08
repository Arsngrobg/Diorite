// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    IO.fs
// Summary: Module consisting of functions that may have side effects
// Author:  Arsngrobg
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang

/// <summary>
/// 	The <c>IO</c> module consists of functions that may have side effects and are deemed <i>unsafe</i>.
/// </summary>
module IO =
    /// <summary>
    ///     Reads the text input from the user using the optional <c>prompt</c> parameter.
    /// </summary>
    /// <param name='prompt'> an optional parameter that is displayed to prompt the user </param>
    /// <exception cref='System.IO.IOException'> An IO error occured </exception>
    /// <exception cref='System.OutOfMemoryException'> There is insufficient memory to allocate a buffer for the
    ///                                                returned string
    /// </exception>
    /// <exception cref='System.ArgumentOutOfRangeException'> The number of characters in the next line of characters is
    ///                                                       greater than System.Int32.MaxValue
    /// </exception>
    /// <remarks> Exceptions copied from System.Console.ReadLine </remarks>
    ///
    let input (prompt: string option): string =
        match prompt with
         | Some(p) ->
             printf $"{p}"
             System.Console.ReadLine ()
         | None -> System.Console.ReadLine ()

    /// <summary>
    ///     Reads in a file in one whole chunk.
    /// </summary>
    /// <param name='path'> the file name or file path to the file </param>
    /// <exception cref='System.ArgumentException'> path is an empty string (<c>""</c>) </exception>
    /// <exception cref='System.ArgumentNullException'> path is <c>null</c> </exception>
    /// <exception cref='System.IO.FileNotFoundException'> the file cannot be found </exception>
    /// <exception cref='System.IO.DirectoryNotFoundException'> the specified path is invalid, such as being on an
    ///                                                         unmapped drive
    /// </exception>
    /// <exception cref='System.IO.IOException'> path includes an incorrect or invalid syntax for the file name,
    ///                                          directory name, or volume label
    /// </exception>
    /// <remarks> Exceptions copied from System.IO.StreamReader </remarks>
    let readFile (path: string): string =
        let fileReader: System.IO.StreamReader = new System.IO.StreamReader (path)
        let src: string = fileReader.ReadToEnd ()
        fileReader.Close ()
        src
