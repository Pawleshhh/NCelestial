namespace NCelestial.Core

open NCelestial.Core.MathHelper

type public JulianDate =
    struct
        val JulianDate: float

        member this.Date =
            0.0

        new (julianDate) = 
            // TODO: Check for max (System.DateTime)
            if julianDate < 0.0 then
                failwith "Julian date cannot be negative"

            { JulianDate = julianDate }
        
        new (dayNumber, fractionOfDay) = 
            if dayNumber |> abs |> fractionalPart <> 0.5 then 
                failwith "Day number must have fractional part equal to 0.5"
            if fractionOfDay |> notInRangeInclusive 0.0 1.0 then
                failwith "Fraction of day must have fractional part only"

            new JulianDate(dayNumber + fractionOfDay)


    end