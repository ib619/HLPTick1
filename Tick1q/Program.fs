open System

//------------------------write your answer function(s) here---------------------//

// top-level subfunctions of polarToCartesianApprox (if any)

// factorial function
let fact n =
    if n = 0
    then 1.0
    else List.reduce (*) [1..float n]

// utility function to find index of element in a list
let findListIndex arr (elem:float) = 
    arr |> List.findIndex ((=)elem)

//--------------helper functions for Sine Taylor Series calculation--------------//

// creates a list of ints in the form [0; 1; 3; .. n]
let taylorSine n = 
    let list = [0..n]
    list |> List.filter (fun x -> (x%2 = 1 || x = 0))

// converts a list from taylorSine to the actual coefficients with powers and factorials
let taylorSineCoefficient (x:float) = 
    let calculate n = 
        if n = 0
        then 0.0
        else
            let denominator = fact n
            (x**float n)/denominator
    calculate

//negates every even index element of a list excluding zero to produce alternating +/- pattern
let negateEven arr =
    let indexing (elem:float) =
        let index = findListIndex arr elem
        let even = (index%2 = 0)
        if even && not (index = 0) then
            -1.0*elem
        else
            elem
    indexing


//-------------helper functions for Cosine Taylor Series calculation-------------//

// creates a list of ints in the form [0; 2; 4; .. n]
let taylorCos n = 
    let list = [0..n]
    list |> List.filter (fun x -> (x%2 = 0))

// converts a list from taylorCos to the actual coefficients with powers and factorials
let taylorCosCoefficient (x:float) = 
    let calculate n = 
        if n = 0
        then 1.0
        else
            let denominator = fact n
            (x**float n)/denominator
    calculate

//negates every odd index element of a list to produce alternating +/- pattern
let negateOdd arr =
    let indexing (elem:float) =
        let index = findListIndex arr elem
        let odd = (index%2 = 1)
        if odd && not (index = 0) then
            -1.0*elem
        else
            elem
    indexing

//-------------------------------Sine Calculation--------------------------------//
let sine (x:float) n = 
    let num_list = taylorSine n
    let coef_list = List.map (taylorSineCoefficient x) num_list
    let coef_list_signed = List.map (negateEven coef_list) coef_list
    List.reduce (+) coef_list_signed

//------------------------------Cosine Calculation-------------------------------//
let cosine (x:float) n = 
    let num_list = taylorCos n
    let coef_list = List.map (taylorCosCoefficient x) num_list
    let coef_list_signed = List.map (negateOdd coef_list) coef_list
    List.reduce (+) coef_list_signed

/// answer to Tick1
// the header given here is correct.
let polarToCartesianApprox (r,theta) n = 
    let sine_theta = sine theta n
    let cos_theta = cosine theta n
    (r*cos_theta, r*sine_theta)


//--------------------testbench code - DO NOT CHANGE-----------------------------//

/// used to make generate testbench data
let testInputs =
    let testPolarCoords = List.allPairs [1.;2.] [1.;2.]
    List.allPairs testPolarCoords [0;1;2;3;10]

/// data showing correct results generated with model answer and given here
let testBenchData =
    [
        ((1.0, 1.0), 0, (1.0, 0.0))       
        ((1.0, 2.0), 0, (1.0, 0.0))        
        ((2.0, 1.0), 0, (2.0, 0.0))        
        ((2.0, 2.0), 0, (2.0, 0.0))        
        ((1.0, 1.0), 1, (1.0, 1.0))        
        ((1.0, 2.0), 1, (1.0, 2.0))        
        ((2.0, 1.0), 1, (2.0, 2.0))        
        ((2.0, 2.0), 1, (2.0, 4.0))        
        ((1.0, 1.0), 2, (0.5, 1.0))        
        ((1.0, 2.0), 2, (-1.0, 2.0))        
        ((2.0, 1.0), 2, (1.0, 2.0))        
        ((2.0, 2.0), 2, (-2.0, 4.0))        
        ((1.0, 1.0), 3, (0.5, 0.8333333333))        
        ((1.0, 2.0), 3, (-1.0, 0.6666666667))        
        ((2.0, 1.0), 3, (1.0, 1.666666667))        
        ((2.0, 2.0), 3, (-2.0, 1.333333333))        
        ((1.0, 1.0), 10, (0.5403023038, 0.8414710097))        
        ((1.0, 2.0), 10, (-0.4161552028, 0.9093474427))        
        ((2.0, 1.0), 10, (1.080604608, 1.682942019))        
        ((2.0, 2.0), 10, (-0.8323104056, 1.818694885))
    ]
/// test testFun with testData to see whether actual results are the same as
/// expected results taken from testData
let testBench testData testFun =
    let closeTo f1 f2 = abs (f1 - f2) < 0.000001
    let testItem fn (coords, n, (expectedX,expectedY) as expected) =
        let actualX,actualY as actual = testFun coords n
        if not (closeTo actualX expectedX) || not (closeTo actualY expectedY) then
            printfn "Error: coords=%A, n=%d, expected result=%A, actual result=%A"coords n expected actual
            1
        else
            0
    printfn "Starting tests..."
    let numErrors = List.sumBy (testItem testFun) testData
    printfn "%d tests Passed %d tests failed." (testData.Length - numErrors) numErrors

[<EntryPoint>]
let main argv =
    testBench testBenchData polarToCartesianApprox
    0 // return an integer exit code
