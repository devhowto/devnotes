export const MODNAME = "e08b-locations";

type Location = "Zurich" | "Oslo" | "Beijing";

function getCountryForLocation(location: Location): string {
  switch(location) {
    case "Zurich":
      return "Switzerland";
    case "Oslo":
      return "Norway";
    default:
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
