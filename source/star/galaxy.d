module star.galaxy;

import star.misc;
import star.octree;
import star.point;
import star.random;
import std.algorithm;
import std.conv;
import std.experimental.logger : infof, tracef;
import std.file;
import std.getopt;
import std.math;
import std.random;
import std.range;
import std.stdio;

alias rndGen = star.random.rndGen;

enum Type
{
    spiral,
    barred,
    elliptical
}

/**
  An Arm is a logarithmic spiral. It starts at some point within the galactic center and ends at
  some radius. It diffuses near the end.
  */
struct Arm
{
    // We can adjust the center to get the start in the required position by calculating the
    // position at the earliest angle.
    Point center = Point(0, 0, 0);
    /// parameters for volume / portion of spiral
    real startAngle, endAngle, thickness;
    /**
      * Parameters for shape of spiral (logarithmic).
      * The spiral is given by polar coordinates: r = a*e^(b*(t+c))
      * a scales the spiral.
      * b makes the spiral tighter (smaller numbers) or looser (bigger numbers).
      * c rotates the spiral. A number greater than 2pi effectively skips the innermost loop; >4pi
      * skips two innermost; etc.
      */
    real a = 1, b = 1, c = 0;

    Point at(real angle)
    {
        if (angle <= endAngle && angle >= startAngle)
        {
            auto radius = (a * E) ^^ (b * (angle + c));
            return Point(sin(angle) * radius, cos(angle) * radius) + center;
        }
        return Point.invalid;
    }

    Point closest(Point other)
    {
        // I don't know of a closed-form solution for this. However, we know that the closest point
        // will have the same relative angle modulo a circle to the given point, assuming the spiral
        // is infinite. Since it's finite, we need to check the start and end points as well.
        // (That might not be correct, but it's reasonably close.)
        auto angle = projectedAngleFromOrigin(other - center);
        real bestDist = real.max;
        Point best;

        void test(Point p)
        {
            auto d = p.dist(other);
            if (d < bestDist)
            {
                bestDist = d;
                best = p;
            }
        }

        test(this.at(startAngle));
        test(this.at(endAngle));
        while (angle < endAngle)
        {
            auto p = this.at(angle);
            test(p);
            angle += 2 * PI;
        }

        if (bestDist < thickness) return other;
        auto delta = best - other;
        return best + (delta * (bestDist / thickness));
    }
}

class Cell
{
    Layer layer;
    Point[] points;
}

class Layer
{
    static Layer
        giants,
        dwarfs;

    static this()
    {
        // Let's say 3% of stars are giants.
        giants = new Layer(1, 8000, 0, -13, 0.03);
        // And 80% are dwarfs large enough to bother with.
        dwarfs = new Layer(2, 500, 10, 0, 0.8);
    }

    ushort id;
    real gridSize;
    real brightnessMinimum, brightnessMaximum;
    real portionOfStarsInLayer;
    real standardDeviation;

    this(ushort id, real gridSize, real brightnessMinimum, real brightnessMaximum,
            real portionOfStarsInLayer)
    {
        this.id = id;
        this.gridSize = gridSize;
        this.brightnessMinimum = brightnessMinimum;
        this.brightnessMaximum = brightnessMaximum;
        this.portionOfStarsInLayer = portionOfStarsInLayer;
    }

    Octree!Star generate(
            Galaxy g, Point loc, Point d, Octree!Star higher, real minBrightnessToSee = 6)
    {
        // Find the bounding volume.
        // We can do this in a closed-form way, but let's do it this way to be safer.
        Star test;
        test.absoluteMagnitude = brightnessMaximum;
        test.loc = Point(0, 0, 0);
        real maxDistance = 100_000;
        for (real dist = 100; dist < 100_000; dist *= E)
        {
            if (test.brightnessAt(dist) > minBrightnessToSee)
            {
                maxDistance = dist;
                break;
            }
        }
        // absolute brightness = apparent brightness + 5 - 5 * log(distance/1pc)
        // we want the max distance to see the star
        // 5log(dist/1pc) = apparent - absolute - 5
        // dist/1pc = e^(apparent/5 - absolute/5 - 1)
        infof("the farthest away you can see a %s star is %s", brightnessMaximum, maxDistance);

        // We need to grab a cone with point angle = 90° and height=maxDistance
        // The sides are maxDistance/cos(45°) long (which happens to be sqrt(2)).
        Point mdp = Point(maxDistance, maxDistance, maxDistance) *  sqrt(cast(real)2.0);
        auto volume = Volume(loc - mdp, loc + mdp);
        auto v2 = Volume(loc - mdp * 1.5, loc + mdp * 1.5);
        auto output = new Octree!Star(v2, maxDistance ^^ 0.5);
        poissonSample!(Star, p => genStar(p))(rndGen, higher, output, volume,
                (g.starSpacing / portionOfStarsInLayer) ^^ (1.0/3));
        infof("finished generating stars on layer %s, volume %s", id, volume);
        return output;
    }

    Star genStar(Point p)
    {
        Star s;
        s.loc = p;
        s.absoluteMagnitude = (brightnessMinimum - brightnessMaximum) / 2 + brightnessMaximum;
        s.temperature = 10000;
        return s;
    }
}

/**
  The Core is always an ellipsoid.
  */
struct Core
{
    real angle;
    /** Axes in sorted order. If it's a barred spiral galaxy, the main arms come from the ends of
     * the core, which means knowing the longest axis first and foremost. */
    real[3] axes;
}

class Galaxy
{
    /// We render the galaxy at this angle.
    real angle;
    Core core;
    Arm[] arms;
    string name;
    real numStars;
    real starSpacing;
    real height;
    real radius;
    Type type;
    ulong seed;

    void sortAxes()
    {
        core.axes[] *= -1;
        sort(core.axes[]);
        core.axes[] *= -1;
    }
}

void printSummary(Galaxy g)
{
    writefln("Galaxy %s (%s)", g.seed, g.name);
    writefln("This is a %s galaxy comprised of roughly %s stars, 1 star per %s ly³", g.type,
            g.numStars, g.starSpacing);
    if (g.type == Type.elliptical)
    {
        writefln("It's an elliptical galaxy measuring %s by %s by %s",
                g.core.axes[0],
                g.core.axes[1],
                g.core.axes[2],
                );
    }
    else
    {
        writefln("It's %s lightyears in radius and %s lightyears thick", g.radius, g.height);
        writefln("The core measures %s by %s by %s",
                g.core.axes[0],
                g.core.axes[1],
                g.core.axes[2],
                );
        writefln("It has %s arms.", g.arms.length);
        foreach (i, arm; g.arms)
        {
            writefln(
                    "arm%s(x) = %s*e^(%s*(x+%s)) range: %s to %s",
                    i, arm.a, arm.b, arm.c, arm.startAngle, arm.endAngle);
            writefln("\tnear end: %s distal end: %s",
                    arm.at(arm.startAngle).dist(Point.origin),
                    arm.at(arm.endAngle).dist(Point.origin)
                    );
        }
    }
}

void overallMap(Galaxy g)
{
    import std.format : format;
    auto f = File(format("map%s %s.svg", g.seed, g.name), "w");
    scope (exit) { f.flush; f.close; }
    auto offset = Point(g.radius * 1.05, g.radius * 1.05);
    real dim = 1000;
    real scale = dim / (2.05 * g.radius);
    f.writefln(`<?xml version="1.0" standalone="no"?>
<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 20010904//EN"
"http://www.w3.org/TR/2001/REC-SVG-20010904/DTD/svg10.dtd">
<svg version="1.0" xmlns="http://www.w3.org/2000/svg" width="%1$s" height="%1$s">
  <rect x="0" y="0" width="%1$s" height="%1$s" />
  <g transform="scale(%4$s)">
    <g transform="translate(%2$s %3$s)">`,
    dim, offset.x, offset.y, scale);
    foreach (i, arm; g.arms)
    {
    f.writefln(`      <circle cx="0" cy="0" r="%s" stroke="#789182" stroke-width="10" />`,
            g.radius);
        f.writefln(`      <!-- arm %s -->`, i);
        for (real angle = arm.startAngle; angle <= arm.endAngle; angle += PI/40)
        {
            auto p = arm.at(angle);
            f.writefln(`      <circle cx="%s" cy="%s" r="%s" fill="#00ff77" />`,
                    p.x, p.y, arm.thickness);
        }
    }
    f.writefln(`      <ellipse fill="yellow" cx="0" cy="0" rx="%s" ry="%s" />`,
            g.core.axes[0], g.core.axes[1]);
    f.writeln(`
    </g>
  </g>
</svg>`);
}


void galaxymain(string[] args)
{
    ulong seed = unpredictableSeed;
    string locArg, dirArg;
    real x, y, z;
    real vx, vy, vz;
    bool verbose;
    real brightness = 6;
    auto opts = getopt(args,
            "s|seed", "random seed", &seed,
            "l|location", "starmap location", &locArg,
            "d|direction", "starmap direction", &dirArg,
            "b|brightness", "minimum star brightness to be visible", &brightness,
            "v|verbose", "use verbose logging", &verbose);
    if (opts.helpWanted)
    {
        defaultGetoptPrinter("generate a galaxy map", opts.options);
        return;
    }
    import std.experimental.logger : sharedLog, LogLevel;
    if (verbose)
    {
        sharedLog.logLevel = LogLevel.info;
    }
    else
    {
        sharedLog.logLevel = LogLevel.warning;
    }
    star.random.rndGen.seed(seed);

    Type type = choice([Type.elliptical, Type.spiral, Type.barred], myRandom);

    // Come up with a distribution for the given galaxy type.
    // A galaxy distribution is a collection of shapes. There are two shapes we're interested in:
    // - ellipse
    // - spiral arm
    // A spiral arm is a cylinder bent to a curve. It tapers at the near end and diffuses at the far
    // end. How do we show this? We want a mathematical function that gives the distance to a curve.
    // Our curve is just a distance function for an angle, eg 712° -> 15000ly.
    // The nearest point candidates would be 360° * x + star's angle

    auto g = new Galaxy;
    g.seed = seed;
    g.type = type;
    import std.string : split, strip;
    auto adjectives = import("adjectives").split("\n")[0..$-1];
    auto nouns = import("animals").split("\n")[0..$-1];
    g.name = choice(adjectives, myRandom).strip ~ " " ~ choice(nouns, myRandom).strip;

    // Pick a random log-scale size of the galaxy. This is between 30 million stars (a dwarf galaxy)
    // and 800 billion stars (double the biggest estimates of the Milky Way).
    g.numStars = E^^uniform(cast(real)17.2, 27.4, myRandom);
    // The Milky Way has about 114 cubic lightyears per star, at the denser estimates. Pick our
    // density.
    g.starSpacing = gaussRandom(120, 20, myRandom);
    // Now we can get the volume.
    auto volume = g.numStars * g.starSpacing;
    if (type == Type.elliptical)
    {
        // avgRadius is slightly wrong, but this ensures that galaxies are generally somewhat flat?
        auto avgRadius = volume ^^ (1.0 / 3);
        g.core.axes[0] = gaussRandom(avgRadius, avgRadius * 0.4, myRandom);
        g.core.axes[1] = gaussRandom(avgRadius, avgRadius * 0.4, myRandom);
        g.core.axes[2] = volume / g.core.axes[0] / g.core.axes[1] / PI * 6;
        g.sortAxes;
        g.radius = g.core.axes[0];
        g.height = g.core.axes[2];
    }
    else
    {
        // Spiral and barred spiral galaxies have a strong aspect ratio.
        // They're squat cylinders.
        // The Milky Way's radius is about 40 times its height.
        // Let's say that's average on a geometric scale.
        auto heightRatio = gaussRandom(6.8, 0.8) ^^ 2;
        // volume = radius² * PI * height
        // volume = (height * heightRatio)² * PI * height
        // volume = height³ * heightRatio² * PI
        // height = cube root(volume / PI / heightRatio²)
        g.height = (volume / PI / heightRatio^^2) ^^ (1.0/3);
        g.radius = g.height * heightRatio;
        auto coreVolume = volume * clampNegative(gaussRandom(0.001, 0.0005));
        auto avgRadius = (coreVolume * PI / 6) ^^ (1.0 / 3);

        if (type == Type.spiral)
        {
            g.core.axes[0] = gaussRandom(1.0, 0.2);
            g.core.axes[1] = gaussRandom(1.0, 0.2);
            g.core.axes[2] = 1;
            normalizeAverage(g.core.axes[]);
            g.core.axes[] *= avgRadius;
        }
        else if (type == Type.barred)
        {
            g.core.axes[0] = gaussRandom(avgRadius, avgRadius * 0.1);
            g.core.axes[1] = gaussRandom(avgRadius, avgRadius * 0.1);
            g.core.axes[2] = coreVolume / g.core.axes[0] / g.core.axes[1] / PI * 6;
        }
        g.sortAxes;

        // Milky Way has about 10 million stars within one parsec of the galactic center. As a
        // whole, though, its bar is roughly 8kpc long and 3kpc wide. About 0.1% of the volume
        // of the Milky Way.
        // Figure that this is average.

        uint armCount = uniform(2U, 10U, myRandom);
        // Now let's build the arms!
        // In a spiral galaxy, the arms can start anywhere.
        // They should have roughly the same parameters.
        Arm prototype;
        prototype.thickness = g.height;
        prototype.startAngle = 0;
        prototype.endAngle = clampNegative(gaussRandom(1.8, 0.4)) * PI;
        prototype.a = uniform(0.1, PI, myRandom);
        prototype.c = uniform(0, PI, myRandom);
        // galaxyRadius = (a * E) ^^ (b * (baseAngleEnd + c))
        // log(a * E, galaxyRadius) = b * (baseAngleEnd + c)
        // log(a * E, galaxyRadius) / (baseAngleEnd + c) = b
        prototype.b = log(g.radius) / log(prototype.a * E) / (prototype.endAngle + prototype.c);
        // start = (a * E) ^^ (b * (startAngle + c))
        // log(a*E, start) = b * (startAngle + c)
        // log(a*E, start)/b - c = startAngle
        // start = coreRadius + thickness + margin
        prototype.startAngle = log(g.core.axes[0] + prototype.thickness * 1.1)
            / log(prototype.a * E) / prototype.b - prototype.c;
        tracef("arm prototype: %s", prototype);


        foreach (i; 0..armCount)
        {
            Arm arm = prototype;
            auto s = (2 * PI / armCount) * i;
            auto jc = gaussRandom(0, 0.1, rndGen);
            auto js = gaussRandom(0, 0.1, rndGen);
            auto je = gaussRandom(0, 0.1, rndGen);
            if (je < 0) je = 0;
            arm.c += s + jc;
            arm.startAngle -= s + js;
            arm.endAngle -= s + je;
            tracef("arm %s jitter: %s, %s, %s", i, jc, js, je);
            /*
            arm.startAngle *= gaussRandom(1, 0.05);
            auto endMult = gaussRandom(1.1, 0.05);
            if (endMult < 1) endMult = 1;
            arm.endAngle -= s * endMult;
            */
            g.arms ~= arm;
        }
    }
    g.core.axes[] *= -1;
    sort(g.core.axes[]);
    g.core.axes[] *= -1;

    printSummary(g);
    overallMap(g);

    tracef("core: ", g.core);
    tracef("galaxy: %s radius, %s height", g.radius, g.height);
    auto loc = locArg.argToPoint(g);
    auto dir = dirArg.argToPoint(g);
    import std.format : format;
    generateStarmap(g, loc, dir, brightness, "starmap %s %s looking to %s.svg"
            .format(g.name, loc, dir));
}

void generateStarmap(Galaxy g, Point loc, Point dir, real minBrightness, string filename)
{
    real height = 1000;
    real halfHeight = height / 2;
    // Our frustum is essentially a pyramid, and its size depends on the layer.
    // Let's look at the entire axis-aligned bounding box and then filter.
    // First, we get the unit vector for our direction:
    auto d = loc - dir;
    d = d / d.magnitude;
    auto dAngleYZ = atan2(d.y, d.z);
    auto dAngleXZ = atan2(d.x, d.z);
    auto dAngleXY = atan2(d.x, d.y);

    infof("projecting to ray %s -> %s", loc, dir);

    // Find our stars
    auto bigStars = Layer.giants.generate(g, loc, d, new Octree!Star(Volume(), 10), minBrightness);
    auto littleStars = Layer.dwarfs.generate(g, loc, d, bigStars, minBrightness);

    auto f = File(filename, "w");
    scope(exit) f.close;
    // Go through and figure out the location for each star in our map.
    f.writefln(`<?xml version="1.0" standalone="no"?>
<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 20010904//EN"
"http://www.w3.org/TR/2001/REC-SVG-20010904/DTD/svg10.dtd">
<svg version="1.0" xmlns="http://www.w3.org/2000/svg" width="%s" height="%s">
  <rect fill="black" x="0" y="0" width="1000" height="1000" />
  <g transform="translate(%s, %s)">`, height, height, halfHeight, halfHeight);

    size_t drawn = 0;

    // Okay, given a location, direction, and object location, where do we draw it on the svg?
    // The viewport is PI/2 radians high and wide. We project each star to a plane normal to the
    // loc-dir line where the hypoteneuse of a right triangle is svg-height units. (Or a unit
    // triangle and multiply. Either way.) Then we just grab everything off that projection --
    // rotate it to be flat and bob's your uncle.
    //
    // Or to figure it another way, we can get an angle (vertical and horizontal) between the
    // look-ray and the star-ray, and then 2*sin gets the coords. I think. Let's try it!
    void draw(Star star)
    {
        auto starloc = star.loc - loc;
        auto dist = starloc.magnitude;
        if (star.brightnessAt(dist) > minBrightness)
        {
            return;
        }
        // Now rotate to put d at (1, 0, 0) and project to unit sphere
        // We do three rotations. The first is around the Z axis. The Z coord stays the same. X and
        // Y rotate to keep the same length, but the angle changes according to d's angle on this
        // plane.
        Point rotateAroundZ = starloc;
        auto lenXY = (starloc.x ^^ 2 + starloc.y ^^ 2) ^^ 0.5;
        auto angleXY = atan2(starloc.x, starloc.y) - dAngleXY;
        rotateAroundZ.x = sin(angleXY) * lenXY;
        rotateAroundZ.y = cos(angleXY) * lenXY;

        Point rotateAroundY = rotateAroundZ;
        auto lenXZ = (rotateAroundZ.x ^^ 2 + rotateAroundZ.z ^^ 2) ^^ 0.5;
        auto angleXZ = atan2(rotateAroundZ.x, rotateAroundZ.z) - dAngleXZ;
        rotateAroundZ.x = sin(angleXZ) * lenXZ;
        rotateAroundZ.z = cos(angleXZ) * lenXZ;

        Point rotateAroundX = rotateAroundY;
        auto lenYZ = (rotateAroundZ.y ^^ 2 + rotateAroundZ.z ^^ 2) ^^ 0.5;
        auto angleYZ = atan2(rotateAroundZ.y, rotateAroundZ.z) - dAngleYZ;
        rotateAroundZ.y = sin(angleYZ) * lenYZ;
        rotateAroundZ.z = cos(angleYZ) * lenYZ;

        auto p = rotateAroundZ;


        // And project further to the x=1 plane
        if (p.x <= 0)
        {
          //  infof("skipping star at %s (projection failed, wrong side)", starloc);
            // can't project it
            return;
        }
        // Now push it out to fit our scale
        p = p / p.x / PI * 1000;

        // Now the y and z coordinates are the x and y coordinates on the svg
        if (abs(p.y) > halfHeight || abs(p.z) > halfHeight)
        {
            // it's not visible
            //infof("skipping star at %s (outside cone)", starloc);
            return;
        }
        f.writefln(`<circle r="0.5" cx="%s" cy="%s" fill="%s" />`, p.y, p.z, star.svgColor(dist));
        drawn++;
        {
            f.flush;
            //infof("drew %s stars, last at %s", drawn, p);
        }
    }

    littleStars.visit(littleStars.volume, (p, s) { draw(s); });
    bigStars.visit(bigStars.volume, (p, s) { draw(s); });
    f.writeln(`
  </g>
</svg>`);
}

struct Star
{
    Point loc;
    real absoluteMagnitude;
    real temperature;

    string svgColor(real distance) { return "white"; }

    real brightnessAt(real distance)
    {
        // distance = (10 parsecs) * 10^^((apparent brightness - absolute magnitude)/5)
        // log10(distance/10pc) * 5 = apparent brightness - absolute magnitude
        // apparent brightness = log10(distance/10pc) * 5 + absolute magnitude
        return log10(distance / (10 * parsecToLightyear)) * 5 + absoluteMagnitude;
    }
}

Point argToPoint(string arg, Galaxy g)
{
    if (arg == null)
    {
        return Point(
                uniform(-g.radius, g.radius, myRandom),
                uniform(-g.radius, g.radius, myRandom),
                uniform(-g.height, g.height, myRandom));
    }
    import std.string : split;
    import std.conv : to;
    auto parts = arg.split(",");
    return Point(parts[0].to!real, parts[1].to!real, parts[2].to!real);
}
