namespace FsCheckSample



module RomanNumeral =
    let arabicToRoman (input:int) =
        (String.replicate input "I")
         .Replace("IIIII", "V")
         .Replace("VV", "X")
         .Replace("XXXXX", "L")
         .Replace("LL", "C")
