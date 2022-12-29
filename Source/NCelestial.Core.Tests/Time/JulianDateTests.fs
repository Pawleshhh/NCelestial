module NCelestial.Core.Tests.JulianDateTests

open System
open NUnit.Framework
open NCelestial.Core

[<TestCase(68569.5, 0.0, 68569.5)>]
[<TestCase(999_999_999.5, 0.5, 1e9)>]
[<TestCase(2136.5, 0.6, 2137.1)>]
let ``JulianDate, pass dayNumber and fraction, properties set as expected`` dayNumber fraction date =
    let julianDate = new JulianDate (dayNumber, fraction)

    Assert.That (julianDate.DayNumber, Is.EqualTo(dayNumber), "Day number")
    Assert.That (julianDate.FractionOfDay, Is.EqualTo(fraction).Within(1e-5), "Fraction of day")
    Assert.That (julianDate.JulianDate, Is.EqualTo(date), "Julian date")

[<TestCase(0.5, 1.0)>]
[<TestCase(20043.5, 1.1)>]
[<TestCase(20043.5, -0.0001)>]
[<TestCase(20043.5, 1.0)>]
[<TestCase(20043.5, 1.1)>]
[<TestCase(20043.5, -0.0001)>]
[<TestCase(999_999_999.5, -0.0001)>]
[<TestCase(999_999_999.5, 1.0)>]
[<TestCase(999_999_999.5, 1.1)>]
let ``JulianDate, pass invalid fraction of day, throws exception`` dayNumber fraction =
    Assert.Throws<exn>(fun _ -> new JulianDate (dayNumber, fraction) |> ignore)
    |> ignore

[<TestCase(10.0)>]
[<TestCase(10.0001)>]
[<TestCase(10.4999)>]
[<TestCase(10.5001)>]
[<TestCase(10.9999)>]
[<TestCase(11.0)>]
let ``JulianDate, pass invalid day number, throws exception`` dayNumber =
    Assert.Throws<exn>(fun _ -> new JulianDate (dayNumber, 0.0) |> ignore)
    |> ignore

[<TestCase(20043.9, 20043.5, 0.4)>]
[<TestCase(68569.5, 68569.5, 0.0)>]
[<TestCase(2137.1, 2136.5, 0.6)>]
[<TestCase(1e9, 999_999_999.5, 0.5)>]
let ``JulianDate, pass julian date, properties set as expected`` date dayNumber fraction =
    let julianDate = new JulianDate (date)

    Assert.That (julianDate.DayNumber, Is.EqualTo(dayNumber), "Day number")
    Assert.That (julianDate.FractionOfDay, Is.EqualTo(fraction).Within(1e-5), "Fraction of day")
    Assert.That (julianDate.JulianDate, Is.EqualTo(date), "Julian date")

[<TestCase(-0.001)>]
[<TestCase(1_000_000_000.1)>]
let ``JulianDate, pass invalid julian date, throws exception`` julianDate =
    Assert.Throws<exn>(fun _ -> new JulianDate (julianDate) |> ignore)
    |> ignore

[<TestCase(30.0, 30.0, 0)>]
[<TestCase(30.1, 30.0, 1)>]
[<TestCase(30.0, 30.1, -1)>]
let ``CompareTo, compare julian date as object, expect value`` date1 date2 (expectedValue: int) =
    let julianDate1 = new JulianDate (date1) :> IComparable
    let julianDate2 = new JulianDate (date2) :> obj

    let result = julianDate1.CompareTo julianDate2

    Assert.That (result, Is.EqualTo(expectedValue))

[<TestCase(30.0, 30.0, 0)>]
[<TestCase(30.1, 30.0, 1)>]
[<TestCase(30.0, 30.1, -1)>]
let ``CompareTo, compare julian date, expect value`` date1 date2 (expectedValue: int) =
    let julianDate1 = new JulianDate (date1) :> IComparable<JulianDate>
    let julianDate2 = new JulianDate (date2)

    let result = julianDate1.CompareTo julianDate2

    Assert.That (result, Is.EqualTo(expectedValue))
    
[<TestCase(30.0, 30.0, true)>]
[<TestCase(30.1, 30.0, false)>]
[<TestCase(30.0, 30.1, false)>]
let ``Equals, check equality with julian date as object, expect true or false`` date1 date2 (expectedValue: bool) =
    let julianDate1 = new JulianDate (date1) :> obj
    let julianDate2 = new JulianDate (date2) :> obj

    let result = julianDate1.Equals julianDate2

    Assert.That (result, Is.EqualTo(expectedValue))

[<TestCase(30.0, 30.0, true)>]
[<TestCase(30.1, 30.0, false)>]
[<TestCase(30.0, 30.1, false)>]
let ``Equals, check equality with julian date, expect true or false`` date1 date2 (expectedValue: bool) =
    let julianDate1 = new JulianDate (date1) :> IEquatable<JulianDate>
    let julianDate2 = new JulianDate (date2)

    let result = julianDate1.Equals julianDate2

    Assert.That (result, Is.EqualTo(expectedValue))

[<TestCase(24156.32, "J", "24156.32 JD")>]
[<TestCase(24156.32, "", "24156.32 JD")>]
[<TestCase(24156.0, "J", "24156.00 JD")>]
[<TestCase(24156.0, "", "24156.00 JD")>]
[<TestCase(24156.314214, "J5", "24156.31421 JD")>]
[<TestCase(24156.32, "D", "24156.32")>]
[<TestCase(24156.0, "D", "24156.00")>]
[<TestCase(24156.314214, "D5", "24156.31421")>]
let ``ToString, expect formatted julian date`` (date: float) (format: string) (expected: string) =
    let julianDate = new JulianDate(date)
    let result = julianDate.ToString(format)
    Assert.That(result, Is.EqualTo(expected))

[<TestCase(24156.32, "24156.32 JD")>]
[<TestCase(24156.0, "24156.00 JD")>]
let ``Object.ToString, expect formatted julian date in default format`` (date: float) (expected: string) =
    let julianDate = new JulianDate(date)
    let result = julianDate.ToString()
    Assert.That(result, Is.EqualTo(expected))

[<TestCase(24156.32, "24156.32 JD")>]
[<TestCase(24156.0, "24156.00 JD")>]
let ``ToString, give null format and expect formatted julian date in default format`` (date: float) (expected: string) =
    let julianDate = new JulianDate(date)
    let result = julianDate.ToString(null)
    Assert.That(result, Is.EqualTo(expected))

[<TestCase("A")>]
[<TestCase("2J")>]
[<TestCase("J10.2")>]
[<TestCase("J-1")>]
[<TestCase("Jx")>]
[<TestCase("2D")>]
[<TestCase("D10.2")>]
[<TestCase("D-1")>]
[<TestCase("Dx")>]
[<TestCase("343")>]
let ``ToString, invalid format so format exception is thrown`` (invalidFormat: string) =
    let julianDate = new JulianDate()
    Assert.Throws<FormatException>(fun () -> julianDate.ToString(invalidFormat) |> ignore) |> ignore