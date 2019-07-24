module star.planet;

import star.point;
import star.random;
import std.algorithm;
import std.array;
import std.conv;
import std.format;
import std.getopt;
import std.math;
import std.random;
import std.range;
import std.stdio;
import std.string;
import std.typecons;
alias rndGen = star.random.rndGen;

enum pi = 3.1415926535;

string toHexString(uint v)
{
    return format("%06x", v);
}

struct Hazard
{
    string name;
    real odds;
}

enum Hazard[] hazards =
[

];

enum plantTaxa =
[
    "Ulmaceae (elm)",
    "Brassica",
    "Ceratophyllum (coontails)",
    "magnoliids (nutmeg, cinnamon, avocado)",
    "Austrobaileyales (essential oils)",
    "Nymphaeales (aquatic, including water lilies)",
    "Trochodendrales (spiral leaves, perfect flowers, winged seeds)",
    "Gunnerales (rhubarb)",
    "Dilleniales (peonies)",
    "Saxifragales (stonecrop, elephant ears)",
    "Vitales (grapes)",
    "Rosales (roses, plums, apples)",
    "malvids (wood sorrel, mango, lime)",
    "Santalales (mistletoe)",
    "Caryophyllales (cacti, beets, carnivorous plants)",
    "Cornales (dogwood, hydrangea)",
    "Ericales (tea, persimmon, brazil nut)",
    "campanulids (carrots, celery, lilac, olive)",
    "lamiids (ash, teak, snapdragon, sesame)",
];

enum animalTaxa =
[
    "annelids",
    "arthropods",
    "bryozoa",
    "cnidaria",
    "echinoderms",
    "molluscs",
    "nematodes",
    "reptiles",
    "birds",
];

struct Glyph
{
    uint index;
    string name;
    string wikiaId;

    string url()
    {
        return "https://vignette.wikia.nocookie.net/stargate/images/%s/%s/0%02d.svg"
            .format(wikiaId[0], wikiaId, index);
    }
}

enum glyphs =
[
    Glyph(2, "man", "6e"),
    Glyph(3, "crab", "ad"),
    Glyph(4, "thresher", "9d"),
    Glyph(5, "sheaf", "b8"),
    Glyph(6, "headdress", "88"),
    Glyph(7, "runner", "7f"),
    Glyph(8, "deertracks", "34"),
    Glyph(9, "snake", "06"),
    Glyph(10, "yoke", "06"),
    Glyph(11, "knife", "53"),
    Glyph(12, "scorpion", "06"),
    Glyph(13, "river", "6d"),
    Glyph(14, "scribe", "45"),
    Glyph(15, "ox", "c3"),
    Glyph(16, "foot", "2b"),
    Glyph(17, "table", "27"),
    Glyph(18, "scythe", "91"),
    Glyph(19, "dog", "fa"),
    Glyph(20, "hoe", "62"),
    Glyph(21, "whip", "db"),
    Glyph(22, "rope", "44"),
    Glyph(23, "shield", "ac"),
    Glyph(24, "goad", "8d"),
    Glyph(25, "drawknife", "2b"),
    Glyph(26, "net", "90"),
    Glyph(27, "mustard", "9a"),
    Glyph(28, "pitcher", "1d"),
    Glyph(29, "snake", "26"),
    Glyph(30, "house", "5a"),
    Glyph(31, "lover", "17"),
    Glyph(32, "beggar", "6a"),
    Glyph(33, "bed", "46"),
    Glyph(34, "water", "63"),
    Glyph(35, "sword", "a4"),
    Glyph(36, "juggler", "19"),
    Glyph(37, "bread", "ce"),
    Glyph(38, "bow", "f1"),
    Glyph(39, "supplicant", "11"),
];

Glyph[] gateAddress()
{
    return partialShuffle(glyphs.dup, 6).take(6).array;
}

class Weather
{
    real wind;
    real windDirection;
    real rainfall;
    real temp;
}

class Star
{
    real brightness;
    int temperature;
    real absoluteMagnitude;
    real distance; // AU
    real luminosity;
    real radius;  // compared to Sol
    real apparentRadius;

    void computeProperties()
    {
        distance = (648_000 * 10 / pi) * 10^^((brightness - absoluteMagnitude)/5);
        luminosity = 2.51 ^^ (4.83 - absoluteMagnitude);
        radius = luminosity ^^ 0.5 / (temperature ^^ 2 / 5778.0 ^^ 2);
        // 1. convert star radius to megameters from Sol radiuses
        // 2. convert star distance to megameters from AU
        // 3. figure out the ratio of the star's diameter to the entire circle of that size from the
        //    planet
        // 4. divide by Sun's numbers
        enum solDiameterMm = 2 * 695.7;
        enum auToMm = 149578.71;
        enum solApparentRadius = solDiameterMm / (auToMm ^^ 2 * pi);
        apparentRadius = (radius * solDiameterMm) / ((distance * auToMm) ^^ 2 * pi);
        apparentRadius /= solApparentRadius;
    }
}

class Planet
{
    uint seed;
    Glyph[] gateAddress;
    real axialTilt = 0;
    real waterPercent;
    real gateLatitude;
    real gateLongitude;
    real yearLength;
    real dayLength;
    uint dayOfYear;
    Weather average;
    Weather[] weather;
    string[] majorFlora, majorFauna, tags;
    Star[] stars;
    Moon[] moons;
    real gravity;
    // The angle of the local star system's orbital plane relative to the galactic plane.
    real orbitAngle;
    Point galacticLocation;
    real dayClosestToGalacticCore;
}

void makeStarmap(Planet planet, uint day, string outfile)
{
    rndGen.seed(planet.seed);
    auto f = File(outfile, "w");
    scope (exit)
    {
        f.flush;
        f.close;
    }

    // Where's the sky pointing?
    // Let's say we see a 150° range from wherever we're pointing.
    // What side of the star are we on? We're looking in the opposite direction.

    void showObject(real absoluteAngleA, real absoluteAngleB, uint color, real size)
    {
        f.writeln(`  <circle cx="%s" cy="%s" r="%s" fill="#%08x" />`,
                );
    }

    f.writeln(`<?xml version="1.0" standalone="no"?>
<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 20010904//EN"
"http://www.w3.org/TR/2001/REC-SVG-20010904/DTD/svg10.dtd">
<svg version="1.0" xmlns="http://www.w3.org/2000/svg" width="1920" height="1200">`);

    // First off, let's see what planets might be visible.
    // We're going to pretend that the planets all have aligned orbits.
    auto numPlanets = cast(uint)clampNegative(gaussRandom(8, 3));
    if (numPlanets > 0)
    {
        auto numRockyPlanets = cast(uint)(0.5 + uniform(numPlanets * 0.3, numPlanets * 0.6));

    }
    // We split them roughly in half.
    foreach (i; 0..numPlanets)
    {
        // find orbit distance, current angle, color, size, albedo, (brightness)
        // that tells us what is visible and what it looks like
    }

    // Now stars. We're going to pretend that our galaxy is elliptical.

    f.writeln(`
</svg>`);
}

Planet makePlanet(uint seed)
{
    auto planet = new Planet;
    planet.seed = seed;

    // Tell us about the stars.
    rndGen.seed(seed + 141);
    int starCount = 1;
    if (uniform01 < 0.1)
    {
        starCount++;
        if (uniform01 < 0.1)
        {
            starCount++;
        }
    }
    foreach (i; 0..starCount)
    {
        auto star = new Star;
        star.temperature = uniform(2000, 12000);
        if (i == 0)
        {
            star.brightness = gaussRandom(-26.7, 1);
        }
        else
        {
            star.brightness = gaussRandom(-12, 4);
        }
        star.absoluteMagnitude = gaussRandom(5, 0.3);
        star.computeProperties();
        planet.stars ~= star;
    }

    // Moons!
    rndGen.seed(seed + 12);
    if (uniform01 < 0.7)
    {
        planet.moons ~= genMoon;
        while (uniform01 < 0.1)
        {
            planet.moons ~= genMoon;
        }
    }

    // Basic planet data.
    rndGen.seed(seed);
    planet.gateAddress = gateAddress;
    // TODO use axial tilt plus latitude to influence weather
    planet.axialTilt = abs(gaussRandom(0, 20));
    planet.waterPercent = abs(gaussRandom(0.82, 0.1));
    if (planet.waterPercent > 0.99) planet.waterPercent = 0.99;
    planet.gateLatitude = gaussRandom(0, 20);
    if (planet.gateLatitude < -85) planet.gateLatitude = -85;
    if (planet.gateLatitude > 85) planet.gateLatitude = 85;
    planet.gateLongitude = uniform(-180, 180);
    planet.majorFlora = randomSample(plantTaxa, uniform(2, 3)).array;
    planet.majorFauna = randomSample(animalTaxa, uniform(2, 3)).array;

    // Orbital and galactic position data (that we can't otherwise calculate)
    planet.dayLength = gaussRandom(24, 1.5);
    planet.gravity = gaussRandom(0.98, 0.08);
    planet.orbitAngle = uniform(0.0, 360.0);
    {
        auto star = planet.stars[0];
        // Now we can calculate the year length.
        // period = 2*pi*r / velocity
        // velocity = sqrt(G * M_star / r)
        // M_star = Sol mass * (star radius ratio)³
        auto distanceMeters = star.distance * 1.4958e11;
        // meters per second
        auto velocity = (6.674e-11 * 1.9885e30 * star.radius / distanceMeters) ^^ 0.5;
        auto period = 2 * pi * distanceMeters / velocity;
        planet.yearLength = period / 60 / 60 / planet.dayLength;
        planet.dayOfYear = uniform(0, cast(uint)planet.yearLength);
    }
    planet.dayClosestToGalacticCore = uniform(0, planet.yearLength);
    stderr.writeln("WARNING: not generating galactic location");

    // For consistency, re-seed to a known state.
    rndGen.seed(seed * 2);
    auto numMonths = cast(int)(planet.yearLength / 30 + 0.5);
    planet.average = new Weather;
    planet.average.temp = gaussRandom(12, 5);
    planet.average.rainfall = clampNegative(gaussRandom(70, 10));
    planet.weather = new Weather[numMonths];
    real tempVariation = abs(gaussRandom(12, 2));
    real rainVariation = abs(gaussRandom(40, 20));
    real rainOffset = uniform01 * pi;
    uint numSegments = uniform(2, 4);
    //real[] segmentWeights = iota(numSegments)
    //  .map!(x => gaussRandom(1.0, 0.1)).array.normalizeAverage;
    real[] segmentWeights = iota(numSegments).map!(x => cast(real)1.0).array.normalizeAverage;
    //real[] segmentLengths = iota(numSegments).map!(x => gaussRandom(1.0, 0.2))
    //  .array.normalizeSum(planet.yearLength);
    real[] segmentLengths = iota(numSegments)
        .map!(x => cast(real)1.0)
        .array
        .normalizeSum(planet.yearLength);
    foreach (i, ref w; planet.weather)
    {
        auto startDay = i * 30;
        real progress = (i * 1.0) / planet.weather.length;
        /*
        foreach (s, len; segmentLengths)
        {
            if (len >= startDay)
            {
                progress += (startDay * 1.0 / len) * segmentWeights[s];
                break;
            }
            progress += segmentWeights[s];
        }
        */
        w = new Weather;
        // * How hot is this area?
        // * How much seasonal variation in temperature?
        w.temp = planet.average.temp + tempVariation * cos(2 * pi * progress);
        // * How much precipitation does it get?
        // * How much seasonal variation in precipitation?
        w.rainfall = planet.average.rainfall +
            rainVariation * cos((2 * pi * progress) + rainOffset);
        if (w.rainfall < 0) w.rainfall = 0;
        // * How windy is it? (Seasonal variation?)
    }
    // * What interesting weather patterns? eg lots of lightning, sleet
    rndGen.seed(seed * 4 + 100);
    if (uniform01 < 0.1) planet.tags ~= "lightning";
    if (uniform01 < 0.1) planet.tags ~= "monsoons";
    if (uniform01 < 0.1) planet.tags ~= "earthquakes";
    if (uniform01 < 0.1) planet.tags ~= "volcanism";
    if (uniform01 < 0.1) planet.tags ~= "active burn/growth cycle";
    if (uniform01 < 0.1) planet.tags ~= "heavy pollen";
    if (uniform01 < 0.1) planet.tags ~= "dangerous megafauna";
    if (uniform01 < 0.1) planet.tags ~= "hail";
    if (uniform01 < 0.1) planet.tags ~= "annoying insects";
    if (uniform01 < 0.1) planet.tags ~= "sapient inhabitants";
    if (uniform01 < 0.1) planet.tags ~= "heavy metals";
    if (uniform01 < 0.1) planet.tags ~= "naquadah";


    return planet;
}

class Moon
{
    real apparentRadius;
    real albedo;
    string color;
    /// Tide strength relative to Earth's
    real tideStrength;
    string[] tags;
}

Moon genMoon()
{
    Moon moon = new Moon; // dammit moon moon
    moon.apparentRadius = clampNegative(gaussRandom(0.9, 0.2));
    moon.albedo = gaussRandom(0.1, 0.07);
    moon.tideStrength = gaussRandom(1.0, 0.1);
    if (uniform01 < 0.1)
    {
        // it's a debris cloud
        // is it a ring?
        if (uniform01 < 0.3)
        {
            moon.tags ~= "ring";
        }
        else
        {
            moon.tags ~= "cloud";
        }
    }
    moon.color = choice(["red", "pink", "yellow", "orange", "brown", "gray", "white"]);
    return moon;
}

enum Format
{
    console,
    bbcode
}

string colorCode(int temperature)
{
    // cyan is dcf1f7 at 12k
    // white is ffffff at 8500
    // yellow is fcfcc9 at 5500
    // orange is f7d9be at 3800
    // red is ef9081 at 2000
    // lerp!
    alias CP = Tuple!(int, "temp", uint, "color");
    static immutable controlPoints = [
        CP(12000, 0xdcf1f7),
        CP(8500, 0xffffff),
        CP(5500, 0xfcfcc9),
        CP(3800, 0xf7d9be),
        CP(2000, 0xef9081),
    ];

    if (temperature >= controlPoints[0].temp)
    {
        return controlPoints[0].color.toHexString;
    }
    if (temperature < controlPoints[$-1].temp)
    {
        return controlPoints[$-1].color.toHexString;
    }
    foreach (i; 0..controlPoints.length - 1)
    {
        auto a = controlPoints[i];
        auto b = controlPoints[i + 1];
        if (temperature < a.temp && temperature >= b.temp)
        {
            temperature -= b.temp;
            auto range = b.temp - a.temp;
            auto percent = (temperature * 1.0) / range;
            auto r = b.color + cast(int)(((a.color & 0xFF0000) - (b.color & 0xFF0000)) * percent);
            auto g = b.color + cast(int)(((a.color & 0xFF00) - (b.color & 0xFF00)) * percent);
            auto B = b.color + cast(int)(((a.color & 0xFF) - (b.color & 0xFF)) * percent);
            return ((r & 0xFF0000) | (g & 0xFF00) | (B & 0xFF)).toHexString;
        }
    }
    return controlPoints[$-1].temp.toHexString;
}

string colorName(int temperature)
{
    if (temperature > 10000)
    {
        return "cyan";
    }
    else if (temperature > 7500)
    {
        return "white";
    }
    else if (temperature > 6000)
    {
        return "pale yellow";
    }
    else if (temperature > 5000)
    {
        return "yellow";
    }
    else if (temperature > 3500)
    {
        return "orange";
    }
    return "red";
}

void print(ulong i, Star star)
{
    writefln(`Star %s:
 * brightness %0.2f
 * absolute magnitude %0.2f
 * distance %0.2fAU
 * temperature %s
 * color %s
 * radius: %0.2f Sol
 * apparent radius: %0.2g Sol`,
        i + 1, star.brightness, star.absoluteMagnitude, star.distance, star.temperature,
        star.temperature.colorName, star.radius, star.apparentRadius);
}

void print(Planet p)
{
    writeln("Planet ", p.seed);
    writeln("Gate address: ", p.gateAddress.map!(x => x.name).join(", "));
    writefln("Gravity: %0.2fg", p.gravity);
    writefln("Axial tilt: %0.1f°", p.axialTilt);
    writefln("Water coverage: %d%%", cast(int)(p.waterPercent * 100));
    writefln("Gate location: %0.2f°N, %0.2f°E", p.gateLatitude, p.gateLongitude);
    /*
    writefln("Galactic location: %0.1f by %0.1f°, %0.2f altitude, angle %0.1f",
            p.coreDistance, p.coreLongitude, p.galacticAltitude, p.orbitAngle);
            */
    foreach (i, star; p.stars)
    {
        print(i, star);
    }
    foreach (i, moon; p.moons)
    {
        writefln(`Moon %s:
 * color %s
 * albedo %0.2f
 * apparent radius %0.2f Earth's moon`,
            i + 1, moon.color, moon.albedo, moon.apparentRadius);
        foreach (tag; moon.tags)
        {
            writeln(" * ", tag);
        }
    }
    auto daysPerMonth = cast(int)(p.yearLength / p.weather.length);
    writefln("Calendar: %.2f day year, %.1f hour day, today is day %s (month %s day %s)",
            p.yearLength, p.dayLength, p.dayOfYear,
            1 + (p.dayOfYear / daysPerMonth), p.dayOfYear % daysPerMonth);
    writeln("Major flora: ", p.majorFlora.join(", "));
    writeln("Major fauna: ", p.majorFauna.join(", "));
    if (p.tags.length) writeln("Additional features: ", p.tags.join(", "));
    print("Average", p.average);
    writeln("---");
    foreach (i, monthWeather; p.weather)
    {
        print("Month %s".format(i + 1), monthWeather);
    }
}

void print(string id, Weather weather)
{
    writefln("%s: temp %s°C, rainfall %smm",
            id,
            cast(int)weather.temp,
            cast(int)weather.rainfall);
}

void bbcode(Planet p)
{
    writeln("[b]Planet ", p.seed, "[/b]");
    writefln("[b]Gate address:[/b] [img]http://ikeran.org/stargateaddr/%s.svg[/img]", p.seed);
    writeln;
    writefln("[b]Axial tilt:[/b] %0.1f°", p.axialTilt);
    writefln("[b]Gravity:[/b] %0.2fg", p.axialTilt);
    writefln("[b]Water coverage:[/b] %d%%", cast(int)(p.waterPercent * 100));
    writefln("[b]Gate location:[/b] %0.2f°N, %0.2f°E", p.gateLatitude, p.gateLongitude);
    foreach (star; p.stars)
    {
        writefln("[b]Star[/b] brightness %s, absolute magnitude %s, distance %sAU, " ~
                "temperature %s, [color=%s]color %s[/color], apparent radius %0.2f Sol",
                star.brightness, star.absoluteMagnitude, star.distance, star.temperature,
                star.temperature.colorCode, star.temperature.colorName, star.apparentRadius);
    }
    auto daysPerMonth = cast(int)(p.yearLength / p.weather.length);
    writefln("[b]Calendar:[/b] %.2f day year, %.1f hour day, today is day %s (month %s day %s)",
            p.yearLength, p.dayLength, p.dayOfYear,
            p.dayOfYear / daysPerMonth, p.dayOfYear % daysPerMonth);
    writeln("[b]Major flora:[/b] ", p.majorFlora.join(", "));
    writeln("[b]Major fauna:[/b] ", p.majorFauna.join(", "));
    if (p.tags.length) writeln("[b]Additional features:[/b] ", p.tags.join(", "));
    writeln("[xtable]{thead}{tr}{th}Month{/th}",
            "{th}Temperature{/th}{th}Rainfall{/th}{/tr}{/thead}{tbody}");
    printbb("Average", p.average);
    foreach (i, monthWeather; p.weather)
    {
        printbb("Month %s".format(i + 1), monthWeather);
    }
    writeln("{/tbody}[/xtable]");
    auto svgFile = "addrs/%s.svg".format(p.seed);
    buildAddressSvg(p.gateAddress, svgFile);
    import std.process : execute;
    execute(["scp", svgFile, "linode:stargateaddrs/"]);
}

void printbb(string id, Weather weather)
{
    writefln("{tr}{td}%s{/td}{td}%s°C{/td}{td}%smm{/td}{/tr}",
            id,
            cast(int)weather.temp,
            cast(int)weather.rainfall);
}

void downloadIfNeeded(Glyph g)
{
    import std.file, std.path;
    import std.net.curl : download;
    mkdirRecurse("glyphs");
    auto p = g.filename;
    if (exists(p)) return;
    download(g.url, p);
}

string filename(Glyph glyph)
{
    import std.path;
    return buildPath("glyphs", glyph.index.to!string ~ ".svg");
}

void buildAddressSvg(Glyph[] glyphs, string outfile)
{
    import arsd.dom, std.file;
    XmlDocument d = new XmlDocument(`<?xml version="1.0" standalone="no"?>
<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 20010904//EN"
"http://www.w3.org/TR/2001/REC-SVG-20010904/DTD/svg10.dtd">
<svg version="1.0" xmlns="http://www.w3.org/2000/svg" width="140" height="25">
</svg>`);
    auto svg = d.querySelector("svg");
    foreach (i, glyph; glyphs)
    {
        XmlDocument doc;
        try
        {
            doc = new XmlDocument(glyph.filename.readText);
        }
        catch (Exception e)
        {
            writefln("failed to read XML glyph for %s", glyph.index);
            throw e;
        }
        auto g = doc.querySelector("g");
        g.attributes["stroke"] = "black";
        g.attributes["stroke-width"] = "20";
        g.attributes["stroke-linecap"] = "round";
        g.attributes["fill"] = "orange";
        g.attributes["transform"] = "translate(%s, 24) scale(0.01, -0.01)"
            .format(cast(int)i * 23 - 3);
        g.parentNode = null;
        svg.addChild(g);
    }
    auto f = File(outfile, "w");
    f.write(d.toPrettyString);
    f.close;
}

void planetmain(string[] args)
{
    foreach (g; glyphs)
    {
        downloadIfNeeded(g);
    }
    auto seed = unpredictableSeed;
    uint count = 1;
    Format format = Format.console;
    auto opts = getopt(args,
            "s|seed", "random number seed", &seed,
            "c|count", "how many planets to create", &count,
            "f|format", "output format", &format);
    if (opts.helpWanted)
    {
        defaultGetoptPrinter("make a stargate planet", opts.options);
        return;
    }
    foreach (i; 0..count)
    {
        auto planet = makePlanet(seed + i);
        if (format == Format.console)
        {
            planet.print;
        }
        else
        {
            planet.bbcode;
        }
    }
}
