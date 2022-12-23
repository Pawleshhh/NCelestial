module internal NCelestial.Core.MathHelper

let fractionalPart (v: float) =
     v - truncate v

let inRangeExclusive min max value =
    min < value && value < max

let inRangeInclusive min max value =
    min <= value && value <= max

let notInRangeExclusive min max value =
    inRangeExclusive min max value
    |> not

let notInRangeInclusive min max value =
    inRangeInclusive min max value
    |> not