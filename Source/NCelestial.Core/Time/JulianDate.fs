namespace NCelestial.Core

open System.Runtime.InteropServices
open NCelestial.Core.MathHelper

type public JulianDate =
    struct
        val JulianDate: float

        member this.DayNumber =  this.JulianDate |> JulianDate.RetrieveDayNumber

        member this.FractionOfDay = this.JulianDate |> JulianDate.RetrieveFractionOfDay

        new (julianDate) = 
            JulianDate.FailIfDateIsNotValid julianDate
            { JulianDate = julianDate }
        
        new (dayNumber, fractionOfDay) = 
            if dayNumber |> abs |> fractionalPart <> 0.5 then 
                failwith "Day number must have fractional part equal to 0.5"
            if fractionOfDay < 0.0 || fractionOfDay >= 1.0 then
                failwith "Fraction of day must have fractional part only and be positive"

            new JulianDate(dayNumber + fractionOfDay)

        member this.JulianYear epoch =
            (this.DayNumber - epoch + this.FractionOfDay) / TimeConstants.DaysPerJulianYear

        member this.JulianCentury epoch =
            (this.DayNumber - epoch + this.FractionOfDay) / TimeConstants.DaysPerJulianCentury

        member this.JulianMillenium epoch =
            (this.DayNumber - epoch + this.FractionOfDay) / TimeConstants.DaysPerJulianMillennium

        static member RetrieveDayNumber julianDate =
            truncate (julianDate - 0.5) + 0.5

        static member RetrieveFractionOfDay julianDate =
            match julianDate - (truncate julianDate) + 0.5 with
            | f when f >= 1.0 -> f - 1.0
            | f -> f

        static member FailIfDateIsNotValid julianDate =
            if julianDate < JulianDate.minValue || julianDate > JulianDate.maxValue then
                failwith "Julian date is out of range"

        static member private minValue = 0.0
        static member private maxValue = 1e9

        static member MinValue = new JulianDate(JulianDate.minValue)
        static member MaxValue = new JulianDate(JulianDate.maxValue)

    end