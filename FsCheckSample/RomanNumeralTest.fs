namespace FsCheckSample

open NUnit.Framework;
open FsCheck;
open FsUnit;
open RomanNumeral;

module RomanNumeralTest = 
    let maxRepetitionProperty char maxRepetition (roman:string) =
        let find = String.replicate (maxRepetition+1) char
        roman.Contains find |> not

    let ``has max one V in the sequence`` roman =
        maxRepetitionProperty "V" 1 roman

    let ``has max four I in the sequence`` roman =
        maxRepetitionProperty "I" 4 roman
        
    let ``has max four X in the sequence`` roman =
        maxRepetitionProperty "X" 4 roman
        
    let ``has max one L in the sequence`` roman =
        maxRepetitionProperty "L" 1 roman
        
    let ``has max four C in the sequence`` roman =
        maxRepetitionProperty "C" 4 roman
        
    let ``has max one D in the sequence`` roman =
        maxRepetitionProperty "D" 1 roman

    let testWithRange f num = 
        // setup a filter
        let romanIsInRange i = (i >= 1) && (i <= 4000)
        // if number is in range then check the property 
        romanIsInRange num ==> lazy (f num)

    [<Test>]
    let ``Test that roman numerlas have no more than one V`` () =
        let property num = 
            arabicToRoman num |> ``has max one V in the sequence``
        Check.QuickThrowOnFailure (testWithRange property)

    
    [<Test>]
    let ``Test that roman numerlas have no more than four I`` () =
        let property num = 
            arabicToRoman num |> ``has max four I in the sequence``
        Check.QuickThrowOnFailure (testWithRange property)
        
    [<Test>]
    let ``Test that roman numerlas have no more than four X`` () =
        let property num = 
            arabicToRoman num |> ``has max four X in the sequence``
        Check.QuickThrowOnFailure (testWithRange property)

    [<Test>]
    let ``Test that roman numerlas have no more than one L`` () =
        let property num = 
            arabicToRoman num |> ``has max one L in the sequence``
        Check.QuickThrowOnFailure (testWithRange property)

    [<Test>]
    let ``Test that roman numerlas have no more than four C`` () =
        let property num = 
            arabicToRoman num |> ``has max four C in the sequence``
        Check.QuickThrowOnFailure (testWithRange property)

    [<Test>]
    let ``Test that roman numerlas have no more than four C the case of 3497`` () =
        let find = String.replicate 5 "C"
        (arabicToRoman 3497).Contains find |> should be False
