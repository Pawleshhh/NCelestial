namespace NCelestial.Core

open System
open NCelestial.Core.MathHelper

[<CustomComparison; CustomEquality>]
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

        static member private RetrieveDayNumber julianDate =
            truncate (julianDate - 0.5) + 0.5

        static member private RetrieveFractionOfDay julianDate =
            match julianDate - (truncate julianDate) + 0.5 with
            | f when f >= 1.0 -> f - 1.0
            | f -> f

        static member private FailIfDateIsNotValid julianDate =
            if julianDate < JulianDate.minValue || julianDate > JulianDate.maxValue then
                failwith "Julian date is out of range"

        static member private minValue = 0.0
        static member private maxValue = 1e9

        static member MinValue = new JulianDate(JulianDate.minValue)
        static member MaxValue = new JulianDate(JulianDate.maxValue)

        override this.Equals obj =
            match obj with
            | :? JulianDate as jd -> (this :> IEquatable<JulianDate>).Equals jd
            | _ -> false

        override this.GetHashCode () =
            this.JulianDate.GetHashCode() * 17

        interface IEquatable<JulianDate> with
            member this.Equals other =
                this.JulianDate.Equals other.JulianDate

        interface IComparable<JulianDate> with
            member this.CompareTo other =
                this.JulianDate.CompareTo other.JulianDate

        interface IComparable with
            member this.CompareTo obj =
                match obj with
                | :? JulianDate as jd -> (this :> IComparable<JulianDate>).CompareTo jd
                | _ -> 0

        static member (+) (x: JulianDate, y: JulianDate) =
            new JulianDate (x.JulianDate + y.JulianDate)

        static member (-) (x: JulianDate, y: JulianDate) =
            new JulianDate (x.JulianDate - y.JulianDate)
            
        static member (*) (x: JulianDate, y: JulianDate) =
            new JulianDate (x.JulianDate * y.JulianDate)

        static member (/) (x: JulianDate, y: JulianDate) =
            new JulianDate (x.JulianDate / y.JulianDate)

        static member op_Equality (x: JulianDate, y: JulianDate) =
            (x :> IEquatable<JulianDate>).Equals y

        static member op_Inequality (x: JulianDate,  y: JulianDate) =
            x = y |> not

        static member op_LessThan (x: JulianDate, y: JulianDate) =
            (x :> IComparable<JulianDate>).CompareTo y = -1

        static member op_LessThanOrEqual (x: JulianDate, y: JulianDate) =
            (x :> IComparable<JulianDate>).CompareTo y <= 0

        static member op_GreaterThan (x: JulianDate, y: JulianDate) =
            (x :> IComparable<JulianDate>).CompareTo y = 1

        static member op_GreaterThanOrEqual (x: JulianDate, y: JulianDate) =
            (x :> IComparable<JulianDate>).CompareTo y >= 0

    end