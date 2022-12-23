﻿module NCelestial.Core.Tests.JulianDateTests

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
    Assert.Throws<Exception>(fun _ -> new JulianDate (dayNumber, fraction) |> ignore)
    |> ignore

[<TestCase(10.0)>]
[<TestCase(10.0001)>]
[<TestCase(10.4999)>]
[<TestCase(10.5001)>]
[<TestCase(10.9999)>]
[<TestCase(11.0)>]
let ``JulianDate, pass invalid day number, throws exception`` dayNumber =
    Assert.Throws<Exception>(fun _ -> new JulianDate (dayNumber, 0.0) |> ignore)
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
    Assert.Throws<Exception>(fun _ -> new JulianDate (julianDate) |> ignore)
    |> ignore