namespace NCelestial.Core

open System
open System.Globalization
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

        interface IConvertible with
            member this.GetTypeCode () = TypeCode.Object
            member this.ToBoolean(provider: IFormatProvider): bool = 
                Convert.ToBoolean this.JulianDate
            member this.ToByte(provider: IFormatProvider): byte = 
                Convert.ToByte this.JulianDate
            member this.ToChar(provider: IFormatProvider): char = 
                Convert.ToChar this.JulianDate
            member this.ToDateTime(provider: IFormatProvider): DateTime = 
                Convert.ToDateTime this.JulianDate
            member this.ToDecimal(provider: IFormatProvider): decimal = 
                Convert.ToDecimal this.JulianDate
            member this.ToDouble(provider: IFormatProvider): float = 
                Convert.ToDouble this.JulianDate
            member this.ToInt16(provider: IFormatProvider): int16 = 
                Convert.ToInt16 this.JulianDate
            member this.ToInt32(provider: IFormatProvider): int = 
                Convert.ToInt32 this.JulianDate
            member this.ToInt64(provider: IFormatProvider): int64 = 
                Convert.ToInt64 this.JulianDate
            member this.ToSByte(provider: IFormatProvider): sbyte = 
                Convert.ToSByte this.JulianDate
            member this.ToSingle(provider: IFormatProvider): float32 = 
                Convert.ToSingle this.JulianDate
            member this.ToString(provider: IFormatProvider): string = 
                Convert.ToString this.JulianDate
            member this.ToType(conversionType: Type, provider: IFormatProvider): obj = 
                Convert.ChangeType(this.JulianDate, conversionType)
            member this.ToUInt16(provider: IFormatProvider): uint16 = 
                Convert.ToUInt16 this.JulianDate
            member this.ToUInt32(provider: IFormatProvider): uint32 = 
                Convert.ToUInt32 this.JulianDate
            member this.ToUInt64(provider: IFormatProvider): uint64 = 
                Convert.ToUInt64 this.JulianDate

        override this.ToString() =
            (this :> IFormattable).ToString("J", CultureInfo.InvariantCulture)
        member this.ToString(format) =
            (this :> IFormattable).ToString(format, CultureInfo.InvariantCulture)

        interface IFormattable with
            member this.ToString(format, formatProvider) =
                try
                    let provider =
                        match formatProvider with
                        | null -> CultureInfo.InvariantCulture :> IFormatProvider
                        | f -> f
                    let (decimalPlaces, actualFormat) =
                        match format with
                        | null -> (2, "J")
                        | f when f.Length = 0 -> (2, "J")
                        | f when f.Length = 1 -> (2, format)
                        | f -> (f[1..] |> int, format)

                    if decimalPlaces < 0 then
                        new FormatException("Decimal places cannot be less than 0") |> raise

                    let createFormattedValue (jd: float) (p: IFormatProvider) =
                        jd.ToString($"F{decimalPlaces}", p)

                    match actualFormat[0] with
                    | 'D' -> createFormattedValue this.JulianDate provider
                    | 'J' -> (createFormattedValue this.JulianDate provider) + " JD"
                    | _ -> new FormatException($"{actualFormat[0]} format is not supported") |> raise
                with
                    | ex -> new FormatException("Invalid format for Julian Date", ex) |> raise

        static member (+) (x: JulianDate, y: JulianDate) =
            new JulianDate (x.JulianDate + y.JulianDate)

        static member (-) (x: JulianDate, y: JulianDate) =
            new JulianDate (x.JulianDate - y.JulianDate)

        static member ( * ) (x: JulianDate, y: JulianDate) =
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