export const MODNAME = "e08c-locations";

type Location = "Zurich" | "Oslo" | "Beijing";

function getCountryForLocation(location: Location): string {
  //
  // Comment out either of the conditions and _exhaustive_ below
  // will fail to type check, helping us make sure we did not forget
  // to cover any of the constituents of Location.
  //
  switch(location) {
    case "Zurich":
      return "Switzerland";
    case "Oslo":
      return "Norway";
    case "Beijing":
      return "China";
    default:
      //
      // We can only assign location to exhaustive if all possible
      // variations of location have already been handled by the
      // cases above, meaning location type was narrowed to never.
      //
      const _exhaustive_: never = location;
      throw new Error(`${location} is not known.`);
  }
}

//
// We don't handle Beijing... But not TS errors or warnings
// are presented to us.
//
getCountryForLocation("Beijing" as Location);

//
// Neither do we handle Kathmandu (capital of Nepal). But no TS
// errors or warnings are presented to us.
//
getCountryForLocation("Kathmandu" as any);

//
// Even though we added Beijing to Location, and added a default
// clause to the switch statement to handle unknown locations, the
// type checker did not help us see that we forgot to add an extra
// clause to the switch statement to handle the new location added.
//
