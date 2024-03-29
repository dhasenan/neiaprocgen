module star.opensimplex;
import std.math;

/*
 * OpenSimplex Noise in Java.
 * by Kurt Spencer
 *
 * v1.1 (October 5, 2014)
 * - Added 2D and 4D implementations.
 * - Proper gradient sets for all dimensions, from a
 *   dimensionally-generalizable scheme with an actual
 *   rhyme and reason behind it.
 * - Removed default permutation array in favor of
 *   default seed.
 * - Changed seed-based constructor to be independent
 *   of any particular randomization library, so results
 *   will be the same when ported to other languages.
 */

struct OpenSimplexNoise
{
    enum real STRETCH_CONSTANT_2D = -0.211324865405187; //(1/Math.sqrt(2+1)-1)/2;
    enum real SQUISH_CONSTANT_2D = 0.366025403784439; //(Math.sqrt(2+1)-1)/2;
    enum real STRETCH_CONSTANT_3D = -1.0 / 6; //(1/Math.sqrt(3+1)-1)/3;
    enum real SQUISH_CONSTANT_3D = 1.0 / 3; //(Math.sqrt(3+1)-1)/3;
    enum real STRETCH_CONSTANT_4D = -0.138196601125011; //(1/Math.sqrt(4+1)-1)/4;
    enum real SQUISH_CONSTANT_4D = 0.309016994374947; //(Math.sqrt(4+1)-1)/4;

    enum real NORM_CONSTANT_2D = 47;
    enum real NORM_CONSTANT_3D = 103;
    enum real NORM_CONSTANT_4D = 30;

    enum long DEFAULT_SEED = 0;

    private short[] perm;
    private short[] permGradIndex3D;

    @disable this();

    public this(short[] perm)
    {
        this.perm = perm;
        permGradIndex3D = new short[256];

        for (long i = 0; i < 256; i++)
        {
            //Since 3D has 24 gradients, simple bitmask won't work, so precompute modulo array.
            permGradIndex3D[i] = cast(short)((perm[i] % (gradients3D.length / 3)) * 3);
        }
    }

    //Initializes the class using a permutation array generated from a 64-bit seed.
    //Generates a proper permutation (i.e. doesn't merely perform N successive pair swaps on a base
    //array)
    //Uses a simple 64-bit LCG.
    public this(long seed)
    {
        perm = new short[256];
        permGradIndex3D = new short[256];
        short[] source = new short[256];
        for (short i = 0; i < 256; i++)
            source[i] = i;
        seed = seed * 6364136223846793005L + 1442695040888963407L;
        seed = seed * 6364136223846793005L + 1442695040888963407L;
        seed = seed * 6364136223846793005L + 1442695040888963407L;
        for (long i = 255; i >= 0; i--)
        {
            seed = seed * 6364136223846793005L + 1442695040888963407L;
            long r = cast(long)((seed + 31) % (i + 1));
            if (r < 0)
                r += (i + 1);
            perm[i] = source[r];
            permGradIndex3D[i] = cast(short)((perm[i] % (gradients3D.length / 3)) * 3);
            source[r] = source[i];
        }
    }

    //2D OpenSimplex Noise.
    public real eval(real x, real y)
    {

        //Place input coordinates onto grid.
        real stretchOffset = (x + y) * STRETCH_CONSTANT_2D;
        real xs = x + stretchOffset;
        real ys = y + stretchOffset;

        //Floor to get grid coordinates of rhombus (stretched square) super-cell origin.
        long xsb = fastFloor(xs);
        long ysb = fastFloor(ys);

        //Skew out to get actual coordinates of rhombus origin. We'll need these later.
        real squishOffset = (xsb + ysb) * SQUISH_CONSTANT_2D;
        real xb = xsb + squishOffset;
        real yb = ysb + squishOffset;

        //Compute grid coordinates relative to rhombus origin.
        real xins = xs - xsb;
        real yins = ys - ysb;

        //Sum those together to get a value that determines which region we're in.
        real inSum = xins + yins;

        //Positions relative to origin polong.
        real dx0 = x - xb;
        real dy0 = y - yb;

        //We'll be defining these inside the next block and using them afterwards.
        real dx_ext, dy_ext;
        long xsv_ext, ysv_ext;

        real value = 0;

        //Contribution (1,0)
        real dx1 = dx0 - 1 - SQUISH_CONSTANT_2D;
        real dy1 = dy0 - 0 - SQUISH_CONSTANT_2D;
        real attn1 = 2 - dx1 * dx1 - dy1 * dy1;
        if (attn1 > 0)
        {
            attn1 *= attn1;
            value += attn1 * attn1 * extrapolate(xsb + 1, ysb + 0, dx1, dy1);
        }

        //Contribution (0,1)
        real dx2 = dx0 - 0 - SQUISH_CONSTANT_2D;
        real dy2 = dy0 - 1 - SQUISH_CONSTANT_2D;
        real attn2 = 2 - dx2 * dx2 - dy2 * dy2;
        if (attn2 > 0)
        {
            attn2 *= attn2;
            value += attn2 * attn2 * extrapolate(xsb + 0, ysb + 1, dx2, dy2);
        }

        if (inSum <= 1)
        { //We're inside the triangle (2-Simplex) at (0,0)
            real zins = 1 - inSum;
            if (zins > xins || zins > yins)
            { //(0,0) is one of the closest two triangular vertices
                if (xins > yins)
                {
                    xsv_ext = xsb + 1;
                    ysv_ext = ysb - 1;
                    dx_ext = dx0 - 1;
                    dy_ext = dy0 + 1;
                }
                else
                {
                    xsv_ext = xsb - 1;
                    ysv_ext = ysb + 1;
                    dx_ext = dx0 + 1;
                    dy_ext = dy0 - 1;
                }
            }
            else
            { //(1,0) and (0,1) are the closest two vertices.
                xsv_ext = xsb + 1;
                ysv_ext = ysb + 1;
                dx_ext = dx0 - 1 - 2 * SQUISH_CONSTANT_2D;
                dy_ext = dy0 - 1 - 2 * SQUISH_CONSTANT_2D;
            }
        }
        else
        { //We're inside the triangle (2-Simplex) at (1,1)
            real zins = 2 - inSum;
            if (zins < xins || zins < yins)
            { //(0,0) is one of the closest two triangular vertices
                if (xins > yins)
                {
                    xsv_ext = xsb + 2;
                    ysv_ext = ysb + 0;
                    dx_ext = dx0 - 2 - 2 * SQUISH_CONSTANT_2D;
                    dy_ext = dy0 + 0 - 2 * SQUISH_CONSTANT_2D;
                }
                else
                {
                    xsv_ext = xsb + 0;
                    ysv_ext = ysb + 2;
                    dx_ext = dx0 + 0 - 2 * SQUISH_CONSTANT_2D;
                    dy_ext = dy0 - 2 - 2 * SQUISH_CONSTANT_2D;
                }
            }
            else
            { //(1,0) and (0,1) are the closest two vertices.
                dx_ext = dx0;
                dy_ext = dy0;
                xsv_ext = xsb;
                ysv_ext = ysb;
            }
            xsb += 1;
            ysb += 1;
            dx0 = dx0 - 1 - 2 * SQUISH_CONSTANT_2D;
            dy0 = dy0 - 1 - 2 * SQUISH_CONSTANT_2D;
        }

        //Contribution (0,0) or (1,1)
        real attn0 = 2 - dx0 * dx0 - dy0 * dy0;
        if (attn0 > 0)
        {
            attn0 *= attn0;
            value += attn0 * attn0 * extrapolate(xsb, ysb, dx0, dy0);
        }

        //Extra Vertex
        real attn_ext = 2 - dx_ext * dx_ext - dy_ext * dy_ext;
        if (attn_ext > 0)
        {
            attn_ext *= attn_ext;
            value += attn_ext * attn_ext * extrapolate(xsv_ext, ysv_ext, dx_ext, dy_ext);
        }

        return value / NORM_CONSTANT_2D;
    }

    //3D OpenSimplex Noise.
    public real eval(real x, real y, real z)
    {

        //Place input coordinates on simplectic honeycomb.
        real stretchOffset = (x + y + z) * STRETCH_CONSTANT_3D;
        real xs = x + stretchOffset;
        real ys = y + stretchOffset;
        real zs = z + stretchOffset;

        //Floor to get simplectic honeycomb coordinates of rhombohedron (stretched cube) super-cell
        //origin.
        long xsb = fastFloor(xs);
        long ysb = fastFloor(ys);
        long zsb = fastFloor(zs);

        //Skew out to get actual coordinates of rhombohedron origin. We'll need these later.
        real squishOffset = (xsb + ysb + zsb) * SQUISH_CONSTANT_3D;
        real xb = xsb + squishOffset;
        real yb = ysb + squishOffset;
        real zb = zsb + squishOffset;

        //Compute simplectic honeycomb coordinates relative to rhombohedral origin.
        real xins = xs - xsb;
        real yins = ys - ysb;
        real zins = zs - zsb;

        //Sum those together to get a value that determines which region we're in.
        real inSum = xins + yins + zins;

        //Positions relative to origin polong.
        real dx0 = x - xb;
        real dy0 = y - yb;
        real dz0 = z - zb;

        //We'll be defining these inside the next block and using them afterwards.
        real dx_ext0, dy_ext0, dz_ext0;
        real dx_ext1, dy_ext1, dz_ext1;
        long xsv_ext0, ysv_ext0, zsv_ext0;
        long xsv_ext1, ysv_ext1, zsv_ext1;

        real value = 0;
        if (inSum <= 1)
        { //We're inside the tetrahedron (3-Simplex) at (0,0,0)

            //Determine which two of (0,0,1), (0,1,0), (1,0,0) are closest.
            byte aPolong = 0x01;
            real aScore = xins;
            byte bPolong = 0x02;
            real bScore = yins;
            if (aScore >= bScore && zins > bScore)
            {
                bScore = zins;
                bPolong = 0x04;
            }
            else if (aScore < bScore && zins > aScore)
            {
                aScore = zins;
                aPolong = 0x04;
            }

            //Now we determine the two lattice polongs not part of the tetrahedron that may
            //contribute.  This depends on the closest two tetrahedral vertices, including (0,0,0)
            real wins = 1 - inSum;

            if (wins > aScore || wins > bScore)
            {
                //(0,0,0) is one of the closest two tetrahedral vertices.
                byte c = (bScore > aScore ? bPolong : aPolong);
                //Our other closest vertex is the closest out of a and b.

                if ((c & 0x01) == 0)
                {
                    xsv_ext0 = xsb - 1;
                    xsv_ext1 = xsb;
                    dx_ext0 = dx0 + 1;
                    dx_ext1 = dx0;
                }
                else
                {
                    xsv_ext0 = xsv_ext1 = xsb + 1;
                    dx_ext0 = dx_ext1 = dx0 - 1;
                }

                if ((c & 0x02) == 0)
                {
                    ysv_ext0 = ysv_ext1 = ysb;
                    dy_ext0 = dy_ext1 = dy0;
                    if ((c & 0x01) == 0)
                    {
                        ysv_ext1 -= 1;
                        dy_ext1 += 1;
                    }
                    else
                    {
                        ysv_ext0 -= 1;
                        dy_ext0 += 1;
                    }
                }
                else
                {
                    ysv_ext0 = ysv_ext1 = ysb + 1;
                    dy_ext0 = dy_ext1 = dy0 - 1;
                }

                if ((c & 0x04) == 0)
                {
                    zsv_ext0 = zsb;
                    zsv_ext1 = zsb - 1;
                    dz_ext0 = dz0;
                    dz_ext1 = dz0 + 1;
                }
                else
                {
                    zsv_ext0 = zsv_ext1 = zsb + 1;
                    dz_ext0 = dz_ext1 = dz0 - 1;
                }
            }
            else
            { //(0,0,0) is not one of the closest two tetrahedral vertices.
                byte c = cast(byte)(aPolong | bPolong);
                //Our two extra vertices are determined by the closest two.

                if ((c & 0x01) == 0)
                {
                    xsv_ext0 = xsb;
                    xsv_ext1 = xsb - 1;
                    dx_ext0 = dx0 - 2 * SQUISH_CONSTANT_3D;
                    dx_ext1 = dx0 + 1 - SQUISH_CONSTANT_3D;
                }
                else
                {
                    xsv_ext0 = xsv_ext1 = xsb + 1;
                    dx_ext0 = dx0 - 1 - 2 * SQUISH_CONSTANT_3D;
                    dx_ext1 = dx0 - 1 - SQUISH_CONSTANT_3D;
                }

                if ((c & 0x02) == 0)
                {
                    ysv_ext0 = ysb;
                    ysv_ext1 = ysb - 1;
                    dy_ext0 = dy0 - 2 * SQUISH_CONSTANT_3D;
                    dy_ext1 = dy0 + 1 - SQUISH_CONSTANT_3D;
                }
                else
                {
                    ysv_ext0 = ysv_ext1 = ysb + 1;
                    dy_ext0 = dy0 - 1 - 2 * SQUISH_CONSTANT_3D;
                    dy_ext1 = dy0 - 1 - SQUISH_CONSTANT_3D;
                }

                if ((c & 0x04) == 0)
                {
                    zsv_ext0 = zsb;
                    zsv_ext1 = zsb - 1;
                    dz_ext0 = dz0 - 2 * SQUISH_CONSTANT_3D;
                    dz_ext1 = dz0 + 1 - SQUISH_CONSTANT_3D;
                }
                else
                {
                    zsv_ext0 = zsv_ext1 = zsb + 1;
                    dz_ext0 = dz0 - 1 - 2 * SQUISH_CONSTANT_3D;
                    dz_ext1 = dz0 - 1 - SQUISH_CONSTANT_3D;
                }
            }

            //Contribution (0,0,0)
            real attn0 = 2 - dx0 * dx0 - dy0 * dy0 - dz0 * dz0;
            if (attn0 > 0)
            {
                attn0 *= attn0;
                value += attn0 * attn0 * extrapolate(xsb + 0, ysb + 0, zsb + 0, dx0, dy0, dz0);
            }

            //Contribution (1,0,0)
            real dx1 = dx0 - 1 - SQUISH_CONSTANT_3D;
            real dy1 = dy0 - 0 - SQUISH_CONSTANT_3D;
            real dz1 = dz0 - 0 - SQUISH_CONSTANT_3D;
            real attn1 = 2 - dx1 * dx1 - dy1 * dy1 - dz1 * dz1;
            if (attn1 > 0)
            {
                attn1 *= attn1;
                value += attn1 * attn1 * extrapolate(xsb + 1, ysb + 0, zsb + 0, dx1, dy1, dz1);
            }

            //Contribution (0,1,0)
            real dx2 = dx0 - 0 - SQUISH_CONSTANT_3D;
            real dy2 = dy0 - 1 - SQUISH_CONSTANT_3D;
            real dz2 = dz1;
            real attn2 = 2 - dx2 * dx2 - dy2 * dy2 - dz2 * dz2;
            if (attn2 > 0)
            {
                attn2 *= attn2;
                value += attn2 * attn2 * extrapolate(xsb + 0, ysb + 1, zsb + 0, dx2, dy2, dz2);
            }

            //Contribution (0,0,1)
            real dx3 = dx2;
            real dy3 = dy1;
            real dz3 = dz0 - 1 - SQUISH_CONSTANT_3D;
            real attn3 = 2 - dx3 * dx3 - dy3 * dy3 - dz3 * dz3;
            if (attn3 > 0)
            {
                attn3 *= attn3;
                value += attn3 * attn3 * extrapolate(xsb + 0, ysb + 0, zsb + 1, dx3, dy3, dz3);
            }
        }
        else if (inSum >= 2)
        { //We're inside the tetrahedron (3-Simplex) at (1,1,1)

            //Determine which two tetrahedral vertices are the closest, out of (1,1,0), (1,0,1),
            //(0,1,1) but not (1,1,1).
            byte aPolong = 0x06;
            real aScore = xins;
            byte bPolong = 0x05;
            real bScore = yins;
            if (aScore <= bScore && zins < bScore)
            {
                bScore = zins;
                bPolong = 0x03;
            }
            else if (aScore > bScore && zins < aScore)
            {
                aScore = zins;
                aPolong = 0x03;
            }

            //Now we determine the two lattice polongs not part of the tetrahedron that may
            //contribute.  This depends on the closest two tetrahedral vertices, including (1,1,1)
            real wins = 3 - inSum;
            if (wins < aScore || wins < bScore)
            {
                //(1,1,1) is one of the closest two tetrahedral vertices.
                byte c = (bScore < aScore ? bPolong : aPolong);
                //Our other closest vertex is the closest out of a and b.

                if ((c & 0x01) != 0)
                {
                    xsv_ext0 = xsb + 2;
                    xsv_ext1 = xsb + 1;
                    dx_ext0 = dx0 - 2 - 3 * SQUISH_CONSTANT_3D;
                    dx_ext1 = dx0 - 1 - 3 * SQUISH_CONSTANT_3D;
                }
                else
                {
                    xsv_ext0 = xsv_ext1 = xsb;
                    dx_ext0 = dx_ext1 = dx0 - 3 * SQUISH_CONSTANT_3D;
                }

                if ((c & 0x02) != 0)
                {
                    ysv_ext0 = ysv_ext1 = ysb + 1;
                    dy_ext0 = dy_ext1 = dy0 - 1 - 3 * SQUISH_CONSTANT_3D;
                    if ((c & 0x01) != 0)
                    {
                        ysv_ext1 += 1;
                        dy_ext1 -= 1;
                    }
                    else
                    {
                        ysv_ext0 += 1;
                        dy_ext0 -= 1;
                    }
                }
                else
                {
                    ysv_ext0 = ysv_ext1 = ysb;
                    dy_ext0 = dy_ext1 = dy0 - 3 * SQUISH_CONSTANT_3D;
                }

                if ((c & 0x04) != 0)
                {
                    zsv_ext0 = zsb + 1;
                    zsv_ext1 = zsb + 2;
                    dz_ext0 = dz0 - 1 - 3 * SQUISH_CONSTANT_3D;
                    dz_ext1 = dz0 - 2 - 3 * SQUISH_CONSTANT_3D;
                }
                else
                {
                    zsv_ext0 = zsv_ext1 = zsb;
                    dz_ext0 = dz_ext1 = dz0 - 3 * SQUISH_CONSTANT_3D;
                }
            }
            else
            { //(1,1,1) is not one of the closest two tetrahedral vertices.
                byte c = cast(byte)(aPolong & bPolong);
                //Our two extra vertices are determined by the closest two.

                if ((c & 0x01) != 0)
                {
                    xsv_ext0 = xsb + 1;
                    xsv_ext1 = xsb + 2;
                    dx_ext0 = dx0 - 1 - SQUISH_CONSTANT_3D;
                    dx_ext1 = dx0 - 2 - 2 * SQUISH_CONSTANT_3D;
                }
                else
                {
                    xsv_ext0 = xsv_ext1 = xsb;
                    dx_ext0 = dx0 - SQUISH_CONSTANT_3D;
                    dx_ext1 = dx0 - 2 * SQUISH_CONSTANT_3D;
                }

                if ((c & 0x02) != 0)
                {
                    ysv_ext0 = ysb + 1;
                    ysv_ext1 = ysb + 2;
                    dy_ext0 = dy0 - 1 - SQUISH_CONSTANT_3D;
                    dy_ext1 = dy0 - 2 - 2 * SQUISH_CONSTANT_3D;
                }
                else
                {
                    ysv_ext0 = ysv_ext1 = ysb;
                    dy_ext0 = dy0 - SQUISH_CONSTANT_3D;
                    dy_ext1 = dy0 - 2 * SQUISH_CONSTANT_3D;
                }

                if ((c & 0x04) != 0)
                {
                    zsv_ext0 = zsb + 1;
                    zsv_ext1 = zsb + 2;
                    dz_ext0 = dz0 - 1 - SQUISH_CONSTANT_3D;
                    dz_ext1 = dz0 - 2 - 2 * SQUISH_CONSTANT_3D;
                }
                else
                {
                    zsv_ext0 = zsv_ext1 = zsb;
                    dz_ext0 = dz0 - SQUISH_CONSTANT_3D;
                    dz_ext1 = dz0 - 2 * SQUISH_CONSTANT_3D;
                }
            }

            //Contribution (1,1,0)
            real dx3 = dx0 - 1 - 2 * SQUISH_CONSTANT_3D;
            real dy3 = dy0 - 1 - 2 * SQUISH_CONSTANT_3D;
            real dz3 = dz0 - 0 - 2 * SQUISH_CONSTANT_3D;
            real attn3 = 2 - dx3 * dx3 - dy3 * dy3 - dz3 * dz3;
            if (attn3 > 0)
            {
                attn3 *= attn3;
                value += attn3 * attn3 * extrapolate(xsb + 1, ysb + 1, zsb + 0, dx3, dy3, dz3);
            }

            //Contribution (1,0,1)
            real dx2 = dx3;
            real dy2 = dy0 - 0 - 2 * SQUISH_CONSTANT_3D;
            real dz2 = dz0 - 1 - 2 * SQUISH_CONSTANT_3D;
            real attn2 = 2 - dx2 * dx2 - dy2 * dy2 - dz2 * dz2;
            if (attn2 > 0)
            {
                attn2 *= attn2;
                value += attn2 * attn2 * extrapolate(xsb + 1, ysb + 0, zsb + 1, dx2, dy2, dz2);
            }

            //Contribution (0,1,1)
            real dx1 = dx0 - 0 - 2 * SQUISH_CONSTANT_3D;
            real dy1 = dy3;
            real dz1 = dz2;
            real attn1 = 2 - dx1 * dx1 - dy1 * dy1 - dz1 * dz1;
            if (attn1 > 0)
            {
                attn1 *= attn1;
                value += attn1 * attn1 * extrapolate(xsb + 0, ysb + 1, zsb + 1, dx1, dy1, dz1);
            }

            //Contribution (1,1,1)
            dx0 = dx0 - 1 - 3 * SQUISH_CONSTANT_3D;
            dy0 = dy0 - 1 - 3 * SQUISH_CONSTANT_3D;
            dz0 = dz0 - 1 - 3 * SQUISH_CONSTANT_3D;
            real attn0 = 2 - dx0 * dx0 - dy0 * dy0 - dz0 * dz0;
            if (attn0 > 0)
            {
                attn0 *= attn0;
                value += attn0 * attn0 * extrapolate(xsb + 1, ysb + 1, zsb + 1, dx0, dy0, dz0);
            }
        }
        else
        { //We're inside the octahedron (Rectified 3-Simplex) in between.
            real aScore;
            byte aPolong;
            bool aIsFurtherSide;
            real bScore;
            byte bPolong;
            bool bIsFurtherSide;

            //Decide between polong (0,0,1) and (1,1,0) as closest
            real p1 = xins + yins;
            if (p1 > 1)
            {
                aScore = p1 - 1;
                aPolong = 0x03;
                aIsFurtherSide = true;
            }
            else
            {
                aScore = 1 - p1;
                aPolong = 0x04;
                aIsFurtherSide = false;
            }

            //Decide between polong (0,1,0) and (1,0,1) as closest
            real p2 = xins + zins;
            if (p2 > 1)
            {
                bScore = p2 - 1;
                bPolong = 0x05;
                bIsFurtherSide = true;
            }
            else
            {
                bScore = 1 - p2;
                bPolong = 0x02;
                bIsFurtherSide = false;
            }

            //The closest out of the two (1,0,0) and (0,1,1) will replace the furthest out of the
            //two decided above, if closer.
            real p3 = yins + zins;
            if (p3 > 1)
            {
                real score = p3 - 1;
                if (aScore <= bScore && aScore < score)
                {
                    aScore = score;
                    aPolong = 0x06;
                    aIsFurtherSide = true;
                }
                else if (aScore > bScore && bScore < score)
                {
                    bScore = score;
                    bPolong = 0x06;
                    bIsFurtherSide = true;
                }
            }
            else
            {
                real score = 1 - p3;
                if (aScore <= bScore && aScore < score)
                {
                    aScore = score;
                    aPolong = 0x01;
                    aIsFurtherSide = false;
                }
                else if (aScore > bScore && bScore < score)
                {
                    bScore = score;
                    bPolong = 0x01;
                    bIsFurtherSide = false;
                }
            }

            //Where each of the two closest polongs are determines how the extra two vertices are
            //calculated.
            if (aIsFurtherSide == bIsFurtherSide)
            {
                if (aIsFurtherSide)
                { //Both closest polongs on (1,1,1) side

                    //One of the two extra polongs is (1,1,1)
                    dx_ext0 = dx0 - 1 - 3 * SQUISH_CONSTANT_3D;
                    dy_ext0 = dy0 - 1 - 3 * SQUISH_CONSTANT_3D;
                    dz_ext0 = dz0 - 1 - 3 * SQUISH_CONSTANT_3D;
                    xsv_ext0 = xsb + 1;
                    ysv_ext0 = ysb + 1;
                    zsv_ext0 = zsb + 1;

                    //Other extra polong is based on the shared axis.
                    byte c = cast(byte)(aPolong & bPolong);
                    if ((c & 0x01) != 0)
                    {
                        dx_ext1 = dx0 - 2 - 2 * SQUISH_CONSTANT_3D;
                        dy_ext1 = dy0 - 2 * SQUISH_CONSTANT_3D;
                        dz_ext1 = dz0 - 2 * SQUISH_CONSTANT_3D;
                        xsv_ext1 = xsb + 2;
                        ysv_ext1 = ysb;
                        zsv_ext1 = zsb;
                    }
                    else if ((c & 0x02) != 0)
                    {
                        dx_ext1 = dx0 - 2 * SQUISH_CONSTANT_3D;
                        dy_ext1 = dy0 - 2 - 2 * SQUISH_CONSTANT_3D;
                        dz_ext1 = dz0 - 2 * SQUISH_CONSTANT_3D;
                        xsv_ext1 = xsb;
                        ysv_ext1 = ysb + 2;
                        zsv_ext1 = zsb;
                    }
                    else
                    {
                        dx_ext1 = dx0 - 2 * SQUISH_CONSTANT_3D;
                        dy_ext1 = dy0 - 2 * SQUISH_CONSTANT_3D;
                        dz_ext1 = dz0 - 2 - 2 * SQUISH_CONSTANT_3D;
                        xsv_ext1 = xsb;
                        ysv_ext1 = ysb;
                        zsv_ext1 = zsb + 2;
                    }
                }
                else
                { //Both closest polongs on (0,0,0) side

                    //One of the two extra polongs is (0,0,0)
                    dx_ext0 = dx0;
                    dy_ext0 = dy0;
                    dz_ext0 = dz0;
                    xsv_ext0 = xsb;
                    ysv_ext0 = ysb;
                    zsv_ext0 = zsb;

                    //Other extra polong is based on the omitted axis.
                    byte c = cast(byte)(aPolong | bPolong);
                    if ((c & 0x01) == 0)
                    {
                        dx_ext1 = dx0 + 1 - SQUISH_CONSTANT_3D;
                        dy_ext1 = dy0 - 1 - SQUISH_CONSTANT_3D;
                        dz_ext1 = dz0 - 1 - SQUISH_CONSTANT_3D;
                        xsv_ext1 = xsb - 1;
                        ysv_ext1 = ysb + 1;
                        zsv_ext1 = zsb + 1;
                    }
                    else if ((c & 0x02) == 0)
                    {
                        dx_ext1 = dx0 - 1 - SQUISH_CONSTANT_3D;
                        dy_ext1 = dy0 + 1 - SQUISH_CONSTANT_3D;
                        dz_ext1 = dz0 - 1 - SQUISH_CONSTANT_3D;
                        xsv_ext1 = xsb + 1;
                        ysv_ext1 = ysb - 1;
                        zsv_ext1 = zsb + 1;
                    }
                    else
                    {
                        dx_ext1 = dx0 - 1 - SQUISH_CONSTANT_3D;
                        dy_ext1 = dy0 - 1 - SQUISH_CONSTANT_3D;
                        dz_ext1 = dz0 + 1 - SQUISH_CONSTANT_3D;
                        xsv_ext1 = xsb + 1;
                        ysv_ext1 = ysb + 1;
                        zsv_ext1 = zsb - 1;
                    }
                }
            }
            else
            { //One polong on (0,0,0) side, one polong on (1,1,1) side
                byte c1, c2;
                if (aIsFurtherSide)
                {
                    c1 = aPolong;
                    c2 = bPolong;
                }
                else
                {
                    c1 = bPolong;
                    c2 = aPolong;
                }

                //One contribution is a permutation of (1,1,-1)
                if ((c1 & 0x01) == 0)
                {
                    dx_ext0 = dx0 + 1 - SQUISH_CONSTANT_3D;
                    dy_ext0 = dy0 - 1 - SQUISH_CONSTANT_3D;
                    dz_ext0 = dz0 - 1 - SQUISH_CONSTANT_3D;
                    xsv_ext0 = xsb - 1;
                    ysv_ext0 = ysb + 1;
                    zsv_ext0 = zsb + 1;
                }
                else if ((c1 & 0x02) == 0)
                {
                    dx_ext0 = dx0 - 1 - SQUISH_CONSTANT_3D;
                    dy_ext0 = dy0 + 1 - SQUISH_CONSTANT_3D;
                    dz_ext0 = dz0 - 1 - SQUISH_CONSTANT_3D;
                    xsv_ext0 = xsb + 1;
                    ysv_ext0 = ysb - 1;
                    zsv_ext0 = zsb + 1;
                }
                else
                {
                    dx_ext0 = dx0 - 1 - SQUISH_CONSTANT_3D;
                    dy_ext0 = dy0 - 1 - SQUISH_CONSTANT_3D;
                    dz_ext0 = dz0 + 1 - SQUISH_CONSTANT_3D;
                    xsv_ext0 = xsb + 1;
                    ysv_ext0 = ysb + 1;
                    zsv_ext0 = zsb - 1;
                }

                //One contribution is a permutation of (0,0,2)
                dx_ext1 = dx0 - 2 * SQUISH_CONSTANT_3D;
                dy_ext1 = dy0 - 2 * SQUISH_CONSTANT_3D;
                dz_ext1 = dz0 - 2 * SQUISH_CONSTANT_3D;
                xsv_ext1 = xsb;
                ysv_ext1 = ysb;
                zsv_ext1 = zsb;
                if ((c2 & 0x01) != 0)
                {
                    dx_ext1 -= 2;
                    xsv_ext1 += 2;
                }
                else if ((c2 & 0x02) != 0)
                {
                    dy_ext1 -= 2;
                    ysv_ext1 += 2;
                }
                else
                {
                    dz_ext1 -= 2;
                    zsv_ext1 += 2;
                }
            }

            //Contribution (1,0,0)
            real dx1 = dx0 - 1 - SQUISH_CONSTANT_3D;
            real dy1 = dy0 - 0 - SQUISH_CONSTANT_3D;
            real dz1 = dz0 - 0 - SQUISH_CONSTANT_3D;
            real attn1 = 2 - dx1 * dx1 - dy1 * dy1 - dz1 * dz1;
            if (attn1 > 0)
            {
                attn1 *= attn1;
                value += attn1 * attn1 * extrapolate(xsb + 1, ysb + 0, zsb + 0, dx1, dy1, dz1);
            }

            //Contribution (0,1,0)
            real dx2 = dx0 - 0 - SQUISH_CONSTANT_3D;
            real dy2 = dy0 - 1 - SQUISH_CONSTANT_3D;
            real dz2 = dz1;
            real attn2 = 2 - dx2 * dx2 - dy2 * dy2 - dz2 * dz2;
            if (attn2 > 0)
            {
                attn2 *= attn2;
                value += attn2 * attn2 * extrapolate(xsb + 0, ysb + 1, zsb + 0, dx2, dy2, dz2);
            }

            //Contribution (0,0,1)
            real dx3 = dx2;
            real dy3 = dy1;
            real dz3 = dz0 - 1 - SQUISH_CONSTANT_3D;
            real attn3 = 2 - dx3 * dx3 - dy3 * dy3 - dz3 * dz3;
            if (attn3 > 0)
            {
                attn3 *= attn3;
                value += attn3 * attn3 * extrapolate(xsb + 0, ysb + 0, zsb + 1, dx3, dy3, dz3);
            }

            //Contribution (1,1,0)
            real dx4 = dx0 - 1 - 2 * SQUISH_CONSTANT_3D;
            real dy4 = dy0 - 1 - 2 * SQUISH_CONSTANT_3D;
            real dz4 = dz0 - 0 - 2 * SQUISH_CONSTANT_3D;
            real attn4 = 2 - dx4 * dx4 - dy4 * dy4 - dz4 * dz4;
            if (attn4 > 0)
            {
                attn4 *= attn4;
                value += attn4 * attn4 * extrapolate(xsb + 1, ysb + 1, zsb + 0, dx4, dy4, dz4);
            }

            //Contribution (1,0,1)
            real dx5 = dx4;
            real dy5 = dy0 - 0 - 2 * SQUISH_CONSTANT_3D;
            real dz5 = dz0 - 1 - 2 * SQUISH_CONSTANT_3D;
            real attn5 = 2 - dx5 * dx5 - dy5 * dy5 - dz5 * dz5;
            if (attn5 > 0)
            {
                attn5 *= attn5;
                value += attn5 * attn5 * extrapolate(xsb + 1, ysb + 0, zsb + 1, dx5, dy5, dz5);
            }

            //Contribution (0,1,1)
            real dx6 = dx0 - 0 - 2 * SQUISH_CONSTANT_3D;
            real dy6 = dy4;
            real dz6 = dz5;
            real attn6 = 2 - dx6 * dx6 - dy6 * dy6 - dz6 * dz6;
            if (attn6 > 0)
            {
                attn6 *= attn6;
                value += attn6 * attn6 * extrapolate(xsb + 0, ysb + 1, zsb + 1, dx6, dy6, dz6);
            }
        }

        //First extra vertex
        real attn_ext0 = 2 - dx_ext0 * dx_ext0 - dy_ext0 * dy_ext0 - dz_ext0 * dz_ext0;
        if (attn_ext0 > 0)
        {
            attn_ext0 *= attn_ext0;
            value += attn_ext0 * attn_ext0 * extrapolate(xsv_ext0, ysv_ext0,
                    zsv_ext0, dx_ext0, dy_ext0, dz_ext0);
        }

        //Second extra vertex
        real attn_ext1 = 2 - dx_ext1 * dx_ext1 - dy_ext1 * dy_ext1 - dz_ext1 * dz_ext1;
        if (attn_ext1 > 0)
        {
            attn_ext1 *= attn_ext1;
            value += attn_ext1 * attn_ext1 * extrapolate(xsv_ext1, ysv_ext1,
                    zsv_ext1, dx_ext1, dy_ext1, dz_ext1);
        }

        return value / NORM_CONSTANT_3D;
    }

    //4D OpenSimplex Noise.
    public real eval(real x, real y, real z, real w)
    {

        //Place input coordinates on simplectic honeycomb.
        real stretchOffset = (x + y + z + w) * STRETCH_CONSTANT_4D;
        real xs = x + stretchOffset;
        real ys = y + stretchOffset;
        real zs = z + stretchOffset;
        real ws = w + stretchOffset;

        //Floor to get simplectic honeycomb coordinates of rhombo-hypercube super-cell origin.
        long xsb = fastFloor(xs);
        long ysb = fastFloor(ys);
        long zsb = fastFloor(zs);
        long wsb = fastFloor(ws);

        //Skew out to get actual coordinates of stretched rhombo-hypercube origin. We'll need these
        //later.
        real squishOffset = (xsb + ysb + zsb + wsb) * SQUISH_CONSTANT_4D;
        real xb = xsb + squishOffset;
        real yb = ysb + squishOffset;
        real zb = zsb + squishOffset;
        real wb = wsb + squishOffset;

        //Compute simplectic honeycomb coordinates relative to rhombo-hypercube origin.
        real xins = xs - xsb;
        real yins = ys - ysb;
        real zins = zs - zsb;
        real wins = ws - wsb;

        //Sum those together to get a value that determines which region we're in.
        real inSum = xins + yins + zins + wins;

        //Positions relative to origin polong.
        real dx0 = x - xb;
        real dy0 = y - yb;
        real dz0 = z - zb;
        real dw0 = w - wb;

        //We'll be defining these inside the next block and using them afterwards.
        real dx_ext0, dy_ext0, dz_ext0, dw_ext0;
        real dx_ext1, dy_ext1, dz_ext1, dw_ext1;
        real dx_ext2, dy_ext2, dz_ext2, dw_ext2;
        long xsv_ext0, ysv_ext0, zsv_ext0, wsv_ext0;
        long xsv_ext1, ysv_ext1, zsv_ext1, wsv_ext1;
        long xsv_ext2, ysv_ext2, zsv_ext2, wsv_ext2;

        real value = 0;
        if (inSum <= 1)
        { //We're inside the pentachoron (4-Simplex) at (0,0,0,0)

            //Determine which two of (0,0,0,1), (0,0,1,0), (0,1,0,0), (1,0,0,0) are closest.
            byte aPolong = 0x01;
            real aScore = xins;
            byte bPolong = 0x02;
            real bScore = yins;
            if (aScore >= bScore && zins > bScore)
            {
                bScore = zins;
                bPolong = 0x04;
            }
            else if (aScore < bScore && zins > aScore)
            {
                aScore = zins;
                aPolong = 0x04;
            }
            if (aScore >= bScore && wins > bScore)
            {
                bScore = wins;
                bPolong = 0x08;
            }
            else if (aScore < bScore && wins > aScore)
            {
                aScore = wins;
                aPolong = 0x08;
            }

            //Now we determine the three lattice polongs not part of the pentachoron that may
            //contribute.  This depends on the closest two pentachoron vertices, including (0,0,0,0)
            real uins = 1 - inSum;
            if (uins > aScore || uins > bScore)
            { //(0,0,0,0) is one of the closest two pentachoron vertices.
                byte c = (bScore > aScore ? bPolong : aPolong);
                //Our other closest vertex is the closest out of a and b.
                if ((c & 0x01) == 0)
                {
                    xsv_ext0 = xsb - 1;
                    xsv_ext1 = xsv_ext2 = xsb;
                    dx_ext0 = dx0 + 1;
                    dx_ext1 = dx_ext2 = dx0;
                }
                else
                {
                    xsv_ext0 = xsv_ext1 = xsv_ext2 = xsb + 1;
                    dx_ext0 = dx_ext1 = dx_ext2 = dx0 - 1;
                }

                if ((c & 0x02) == 0)
                {
                    ysv_ext0 = ysv_ext1 = ysv_ext2 = ysb;
                    dy_ext0 = dy_ext1 = dy_ext2 = dy0;
                    if ((c & 0x01) == 0x01)
                    {
                        ysv_ext0 -= 1;
                        dy_ext0 += 1;
                    }
                    else
                    {
                        ysv_ext1 -= 1;
                        dy_ext1 += 1;
                    }
                }
                else
                {
                    ysv_ext0 = ysv_ext1 = ysv_ext2 = ysb + 1;
                    dy_ext0 = dy_ext1 = dy_ext2 = dy0 - 1;
                }

                if ((c & 0x04) == 0)
                {
                    zsv_ext0 = zsv_ext1 = zsv_ext2 = zsb;
                    dz_ext0 = dz_ext1 = dz_ext2 = dz0;
                    if ((c & 0x03) != 0)
                    {
                        if ((c & 0x03) == 0x03)
                        {
                            zsv_ext0 -= 1;
                            dz_ext0 += 1;
                        }
                        else
                        {
                            zsv_ext1 -= 1;
                            dz_ext1 += 1;
                        }
                    }
                    else
                    {
                        zsv_ext2 -= 1;
                        dz_ext2 += 1;
                    }
                }
                else
                {
                    zsv_ext0 = zsv_ext1 = zsv_ext2 = zsb + 1;
                    dz_ext0 = dz_ext1 = dz_ext2 = dz0 - 1;
                }

                if ((c & 0x08) == 0)
                {
                    wsv_ext0 = wsv_ext1 = wsb;
                    wsv_ext2 = wsb - 1;
                    dw_ext0 = dw_ext1 = dw0;
                    dw_ext2 = dw0 + 1;
                }
                else
                {
                    wsv_ext0 = wsv_ext1 = wsv_ext2 = wsb + 1;
                    dw_ext0 = dw_ext1 = dw_ext2 = dw0 - 1;
                }
            }
            else
            { //(0,0,0,0) is not one of the closest two pentachoron vertices.
                byte c = cast(byte)(aPolong | bPolong);
                //Our three extra vertices are determined by the closest two.

                if ((c & 0x01) == 0)
                {
                    xsv_ext0 = xsv_ext2 = xsb;
                    xsv_ext1 = xsb - 1;
                    dx_ext0 = dx0 - 2 * SQUISH_CONSTANT_4D;
                    dx_ext1 = dx0 + 1 - SQUISH_CONSTANT_4D;
                    dx_ext2 = dx0 - SQUISH_CONSTANT_4D;
                }
                else
                {
                    xsv_ext0 = xsv_ext1 = xsv_ext2 = xsb + 1;
                    dx_ext0 = dx0 - 1 - 2 * SQUISH_CONSTANT_4D;
                    dx_ext1 = dx_ext2 = dx0 - 1 - SQUISH_CONSTANT_4D;
                }

                if ((c & 0x02) == 0)
                {
                    ysv_ext0 = ysv_ext1 = ysv_ext2 = ysb;
                    dy_ext0 = dy0 - 2 * SQUISH_CONSTANT_4D;
                    dy_ext1 = dy_ext2 = dy0 - SQUISH_CONSTANT_4D;
                    if ((c & 0x01) == 0x01)
                    {
                        ysv_ext1 -= 1;
                        dy_ext1 += 1;
                    }
                    else
                    {
                        ysv_ext2 -= 1;
                        dy_ext2 += 1;
                    }
                }
                else
                {
                    ysv_ext0 = ysv_ext1 = ysv_ext2 = ysb + 1;
                    dy_ext0 = dy0 - 1 - 2 * SQUISH_CONSTANT_4D;
                    dy_ext1 = dy_ext2 = dy0 - 1 - SQUISH_CONSTANT_4D;
                }

                if ((c & 0x04) == 0)
                {
                    zsv_ext0 = zsv_ext1 = zsv_ext2 = zsb;
                    dz_ext0 = dz0 - 2 * SQUISH_CONSTANT_4D;
                    dz_ext1 = dz_ext2 = dz0 - SQUISH_CONSTANT_4D;
                    if ((c & 0x03) == 0x03)
                    {
                        zsv_ext1 -= 1;
                        dz_ext1 += 1;
                    }
                    else
                    {
                        zsv_ext2 -= 1;
                        dz_ext2 += 1;
                    }
                }
                else
                {
                    zsv_ext0 = zsv_ext1 = zsv_ext2 = zsb + 1;
                    dz_ext0 = dz0 - 1 - 2 * SQUISH_CONSTANT_4D;
                    dz_ext1 = dz_ext2 = dz0 - 1 - SQUISH_CONSTANT_4D;
                }

                if ((c & 0x08) == 0)
                {
                    wsv_ext0 = wsv_ext1 = wsb;
                    wsv_ext2 = wsb - 1;
                    dw_ext0 = dw0 - 2 * SQUISH_CONSTANT_4D;
                    dw_ext1 = dw0 - SQUISH_CONSTANT_4D;
                    dw_ext2 = dw0 + 1 - SQUISH_CONSTANT_4D;
                }
                else
                {
                    wsv_ext0 = wsv_ext1 = wsv_ext2 = wsb + 1;
                    dw_ext0 = dw0 - 1 - 2 * SQUISH_CONSTANT_4D;
                    dw_ext1 = dw_ext2 = dw0 - 1 - SQUISH_CONSTANT_4D;
                }
            }

            //Contribution (0,0,0,0)
            real attn0 = 2 - dx0 * dx0 - dy0 * dy0 - dz0 * dz0 - dw0 * dw0;
            if (attn0 > 0)
            {
                attn0 *= attn0;
                value += attn0 * attn0 * extrapolate(xsb + 0, ysb + 0, zsb + 0,
                        wsb + 0, dx0, dy0, dz0, dw0);
            }

            //Contribution (1,0,0,0)
            real dx1 = dx0 - 1 - SQUISH_CONSTANT_4D;
            real dy1 = dy0 - 0 - SQUISH_CONSTANT_4D;
            real dz1 = dz0 - 0 - SQUISH_CONSTANT_4D;
            real dw1 = dw0 - 0 - SQUISH_CONSTANT_4D;
            real attn1 = 2 - dx1 * dx1 - dy1 * dy1 - dz1 * dz1 - dw1 * dw1;
            if (attn1 > 0)
            {
                attn1 *= attn1;
                value += attn1 * attn1 * extrapolate(xsb + 1, ysb + 0, zsb + 0,
                        wsb + 0, dx1, dy1, dz1, dw1);
            }

            //Contribution (0,1,0,0)
            real dx2 = dx0 - 0 - SQUISH_CONSTANT_4D;
            real dy2 = dy0 - 1 - SQUISH_CONSTANT_4D;
            real dz2 = dz1;
            real dw2 = dw1;
            real attn2 = 2 - dx2 * dx2 - dy2 * dy2 - dz2 * dz2 - dw2 * dw2;
            if (attn2 > 0)
            {
                attn2 *= attn2;
                value += attn2 * attn2 * extrapolate(xsb + 0, ysb + 1, zsb + 0,
                        wsb + 0, dx2, dy2, dz2, dw2);
            }

            //Contribution (0,0,1,0)
            real dx3 = dx2;
            real dy3 = dy1;
            real dz3 = dz0 - 1 - SQUISH_CONSTANT_4D;
            real dw3 = dw1;
            real attn3 = 2 - dx3 * dx3 - dy3 * dy3 - dz3 * dz3 - dw3 * dw3;
            if (attn3 > 0)
            {
                attn3 *= attn3;
                value += attn3 * attn3 * extrapolate(xsb + 0, ysb + 0, zsb + 1,
                        wsb + 0, dx3, dy3, dz3, dw3);
            }

            //Contribution (0,0,0,1)
            real dx4 = dx2;
            real dy4 = dy1;
            real dz4 = dz1;
            real dw4 = dw0 - 1 - SQUISH_CONSTANT_4D;
            real attn4 = 2 - dx4 * dx4 - dy4 * dy4 - dz4 * dz4 - dw4 * dw4;
            if (attn4 > 0)
            {
                attn4 *= attn4;
                value += attn4 * attn4 * extrapolate(xsb + 0, ysb + 0, zsb + 0,
                        wsb + 1, dx4, dy4, dz4, dw4);
            }
        }
        else if (inSum >= 3)
        { //We're inside the pentachoron (4-Simplex) at (1,1,1,1)
            //Determine which two of (1,1,1,0), (1,1,0,1), (1,0,1,1), (0,1,1,1) are closest.
            byte aPolong = 0x0E;
            real aScore = xins;
            byte bPolong = 0x0D;
            real bScore = yins;
            if (aScore <= bScore && zins < bScore)
            {
                bScore = zins;
                bPolong = 0x0B;
            }
            else if (aScore > bScore && zins < aScore)
            {
                aScore = zins;
                aPolong = 0x0B;
            }
            if (aScore <= bScore && wins < bScore)
            {
                bScore = wins;
                bPolong = 0x07;
            }
            else if (aScore > bScore && wins < aScore)
            {
                aScore = wins;
                aPolong = 0x07;
            }

            //Now we determine the three lattice polongs not part of the pentachoron that may
            //contribute.  This depends on the closest two pentachoron vertices, including (0,0,0,0)
            real uins = 4 - inSum;
            if (uins < aScore || uins < bScore)
            { //(1,1,1,1) is one of the closest two pentachoron vertices.
                byte c = (bScore < aScore ? bPolong : aPolong);
                //Our other closest vertex is the closest out of a and b.

                if ((c & 0x01) != 0)
                {
                    xsv_ext0 = xsb + 2;
                    xsv_ext1 = xsv_ext2 = xsb + 1;
                    dx_ext0 = dx0 - 2 - 4 * SQUISH_CONSTANT_4D;
                    dx_ext1 = dx_ext2 = dx0 - 1 - 4 * SQUISH_CONSTANT_4D;
                }
                else
                {
                    xsv_ext0 = xsv_ext1 = xsv_ext2 = xsb;
                    dx_ext0 = dx_ext1 = dx_ext2 = dx0 - 4 * SQUISH_CONSTANT_4D;
                }

                if ((c & 0x02) != 0)
                {
                    ysv_ext0 = ysv_ext1 = ysv_ext2 = ysb + 1;
                    dy_ext0 = dy_ext1 = dy_ext2 = dy0 - 1 - 4 * SQUISH_CONSTANT_4D;
                    if ((c & 0x01) != 0)
                    {
                        ysv_ext1 += 1;
                        dy_ext1 -= 1;
                    }
                    else
                    {
                        ysv_ext0 += 1;
                        dy_ext0 -= 1;
                    }
                }
                else
                {
                    ysv_ext0 = ysv_ext1 = ysv_ext2 = ysb;
                    dy_ext0 = dy_ext1 = dy_ext2 = dy0 - 4 * SQUISH_CONSTANT_4D;
                }

                if ((c & 0x04) != 0)
                {
                    zsv_ext0 = zsv_ext1 = zsv_ext2 = zsb + 1;
                    dz_ext0 = dz_ext1 = dz_ext2 = dz0 - 1 - 4 * SQUISH_CONSTANT_4D;
                    if ((c & 0x03) != 0x03)
                    {
                        if ((c & 0x03) == 0)
                        {
                            zsv_ext0 += 1;
                            dz_ext0 -= 1;
                        }
                        else
                        {
                            zsv_ext1 += 1;
                            dz_ext1 -= 1;
                        }
                    }
                    else
                    {
                        zsv_ext2 += 1;
                        dz_ext2 -= 1;
                    }
                }
                else
                {
                    zsv_ext0 = zsv_ext1 = zsv_ext2 = zsb;
                    dz_ext0 = dz_ext1 = dz_ext2 = dz0 - 4 * SQUISH_CONSTANT_4D;
                }

                if ((c & 0x08) != 0)
                {
                    wsv_ext0 = wsv_ext1 = wsb + 1;
                    wsv_ext2 = wsb + 2;
                    dw_ext0 = dw_ext1 = dw0 - 1 - 4 * SQUISH_CONSTANT_4D;
                    dw_ext2 = dw0 - 2 - 4 * SQUISH_CONSTANT_4D;
                }
                else
                {
                    wsv_ext0 = wsv_ext1 = wsv_ext2 = wsb;
                    dw_ext0 = dw_ext1 = dw_ext2 = dw0 - 4 * SQUISH_CONSTANT_4D;
                }
            }
            else
            { //(1,1,1,1) is not one of the closest two pentachoron vertices.
                byte c = cast(byte)(aPolong & bPolong);
                //Our three extra vertices are determined by the closest two.

                if ((c & 0x01) != 0)
                {
                    xsv_ext0 = xsv_ext2 = xsb + 1;
                    xsv_ext1 = xsb + 2;
                    dx_ext0 = dx0 - 1 - 2 * SQUISH_CONSTANT_4D;
                    dx_ext1 = dx0 - 2 - 3 * SQUISH_CONSTANT_4D;
                    dx_ext2 = dx0 - 1 - 3 * SQUISH_CONSTANT_4D;
                }
                else
                {
                    xsv_ext0 = xsv_ext1 = xsv_ext2 = xsb;
                    dx_ext0 = dx0 - 2 * SQUISH_CONSTANT_4D;
                    dx_ext1 = dx_ext2 = dx0 - 3 * SQUISH_CONSTANT_4D;
                }

                if ((c & 0x02) != 0)
                {
                    ysv_ext0 = ysv_ext1 = ysv_ext2 = ysb + 1;
                    dy_ext0 = dy0 - 1 - 2 * SQUISH_CONSTANT_4D;
                    dy_ext1 = dy_ext2 = dy0 - 1 - 3 * SQUISH_CONSTANT_4D;
                    if ((c & 0x01) != 0)
                    {
                        ysv_ext2 += 1;
                        dy_ext2 -= 1;
                    }
                    else
                    {
                        ysv_ext1 += 1;
                        dy_ext1 -= 1;
                    }
                }
                else
                {
                    ysv_ext0 = ysv_ext1 = ysv_ext2 = ysb;
                    dy_ext0 = dy0 - 2 * SQUISH_CONSTANT_4D;
                    dy_ext1 = dy_ext2 = dy0 - 3 * SQUISH_CONSTANT_4D;
                }

                if ((c & 0x04) != 0)
                {
                    zsv_ext0 = zsv_ext1 = zsv_ext2 = zsb + 1;
                    dz_ext0 = dz0 - 1 - 2 * SQUISH_CONSTANT_4D;
                    dz_ext1 = dz_ext2 = dz0 - 1 - 3 * SQUISH_CONSTANT_4D;
                    if ((c & 0x03) != 0)
                    {
                        zsv_ext2 += 1;
                        dz_ext2 -= 1;
                    }
                    else
                    {
                        zsv_ext1 += 1;
                        dz_ext1 -= 1;
                    }
                }
                else
                {
                    zsv_ext0 = zsv_ext1 = zsv_ext2 = zsb;
                    dz_ext0 = dz0 - 2 * SQUISH_CONSTANT_4D;
                    dz_ext1 = dz_ext2 = dz0 - 3 * SQUISH_CONSTANT_4D;
                }

                if ((c & 0x08) != 0)
                {
                    wsv_ext0 = wsv_ext1 = wsb + 1;
                    wsv_ext2 = wsb + 2;
                    dw_ext0 = dw0 - 1 - 2 * SQUISH_CONSTANT_4D;
                    dw_ext1 = dw0 - 1 - 3 * SQUISH_CONSTANT_4D;
                    dw_ext2 = dw0 - 2 - 3 * SQUISH_CONSTANT_4D;
                }
                else
                {
                    wsv_ext0 = wsv_ext1 = wsv_ext2 = wsb;
                    dw_ext0 = dw0 - 2 * SQUISH_CONSTANT_4D;
                    dw_ext1 = dw_ext2 = dw0 - 3 * SQUISH_CONSTANT_4D;
                }
            }

            //Contribution (1,1,1,0)
            real dx4 = dx0 - 1 - 3 * SQUISH_CONSTANT_4D;
            real dy4 = dy0 - 1 - 3 * SQUISH_CONSTANT_4D;
            real dz4 = dz0 - 1 - 3 * SQUISH_CONSTANT_4D;
            real dw4 = dw0 - 3 * SQUISH_CONSTANT_4D;
            real attn4 = 2 - dx4 * dx4 - dy4 * dy4 - dz4 * dz4 - dw4 * dw4;
            if (attn4 > 0)
            {
                attn4 *= attn4;
                value += attn4 * attn4 * extrapolate(xsb + 1, ysb + 1, zsb + 1,
                        wsb + 0, dx4, dy4, dz4, dw4);
            }

            //Contribution (1,1,0,1)
            real dx3 = dx4;
            real dy3 = dy4;
            real dz3 = dz0 - 3 * SQUISH_CONSTANT_4D;
            real dw3 = dw0 - 1 - 3 * SQUISH_CONSTANT_4D;
            real attn3 = 2 - dx3 * dx3 - dy3 * dy3 - dz3 * dz3 - dw3 * dw3;
            if (attn3 > 0)
            {
                attn3 *= attn3;
                value += attn3 * attn3 * extrapolate(xsb + 1, ysb + 1, zsb + 0,
                        wsb + 1, dx3, dy3, dz3, dw3);
            }

            //Contribution (1,0,1,1)
            real dx2 = dx4;
            real dy2 = dy0 - 3 * SQUISH_CONSTANT_4D;
            real dz2 = dz4;
            real dw2 = dw3;
            real attn2 = 2 - dx2 * dx2 - dy2 * dy2 - dz2 * dz2 - dw2 * dw2;
            if (attn2 > 0)
            {
                attn2 *= attn2;
                value += attn2 * attn2 * extrapolate(xsb + 1, ysb + 0, zsb + 1,
                        wsb + 1, dx2, dy2, dz2, dw2);
            }

            //Contribution (0,1,1,1)
            real dx1 = dx0 - 3 * SQUISH_CONSTANT_4D;
            real dz1 = dz4;
            real dy1 = dy4;
            real dw1 = dw3;
            real attn1 = 2 - dx1 * dx1 - dy1 * dy1 - dz1 * dz1 - dw1 * dw1;
            if (attn1 > 0)
            {
                attn1 *= attn1;
                value += attn1 * attn1 * extrapolate(xsb + 0, ysb + 1, zsb + 1,
                        wsb + 1, dx1, dy1, dz1, dw1);
            }

            //Contribution (1,1,1,1)
            dx0 = dx0 - 1 - 4 * SQUISH_CONSTANT_4D;
            dy0 = dy0 - 1 - 4 * SQUISH_CONSTANT_4D;
            dz0 = dz0 - 1 - 4 * SQUISH_CONSTANT_4D;
            dw0 = dw0 - 1 - 4 * SQUISH_CONSTANT_4D;
            real attn0 = 2 - dx0 * dx0 - dy0 * dy0 - dz0 * dz0 - dw0 * dw0;
            if (attn0 > 0)
            {
                attn0 *= attn0;
                value += attn0 * attn0 * extrapolate(xsb + 1, ysb + 1, zsb + 1,
                        wsb + 1, dx0, dy0, dz0, dw0);
            }
        }
        else if (inSum <= 2)
        { //We're inside the first dispentachoron (Rectified 4-Simplex)
            real aScore;
            byte aPolong;
            bool aIsBiggerSide = true;
            real bScore;
            byte bPolong;
            bool bIsBiggerSide = true;

            //Decide between (1,1,0,0) and (0,0,1,1)
            if (xins + yins > zins + wins)
            {
                aScore = xins + yins;
                aPolong = 0x03;
            }
            else
            {
                aScore = zins + wins;
                aPolong = 0x0C;
            }

            //Decide between (1,0,1,0) and (0,1,0,1)
            if (xins + zins > yins + wins)
            {
                bScore = xins + zins;
                bPolong = 0x05;
            }
            else
            {
                bScore = yins + wins;
                bPolong = 0x0A;
            }

            //Closer between (1,0,0,1) and (0,1,1,0) will replace the further of a and b, if closer.
            if (xins + wins > yins + zins)
            {
                real score = xins + wins;
                if (aScore >= bScore && score > bScore)
                {
                    bScore = score;
                    bPolong = 0x09;
                }
                else if (aScore < bScore && score > aScore)
                {
                    aScore = score;
                    aPolong = 0x09;
                }
            }
            else
            {
                real score = yins + zins;
                if (aScore >= bScore && score > bScore)
                {
                    bScore = score;
                    bPolong = 0x06;
                }
                else if (aScore < bScore && score > aScore)
                {
                    aScore = score;
                    aPolong = 0x06;
                }
            }

            //Decide if (1,0,0,0) is closer.
            real p1 = 2 - inSum + xins;
            if (aScore >= bScore && p1 > bScore)
            {
                bScore = p1;
                bPolong = 0x01;
                bIsBiggerSide = false;
            }
            else if (aScore < bScore && p1 > aScore)
            {
                aScore = p1;
                aPolong = 0x01;
                aIsBiggerSide = false;
            }

            //Decide if (0,1,0,0) is closer.
            real p2 = 2 - inSum + yins;
            if (aScore >= bScore && p2 > bScore)
            {
                bScore = p2;
                bPolong = 0x02;
                bIsBiggerSide = false;
            }
            else if (aScore < bScore && p2 > aScore)
            {
                aScore = p2;
                aPolong = 0x02;
                aIsBiggerSide = false;
            }

            //Decide if (0,0,1,0) is closer.
            real p3 = 2 - inSum + zins;
            if (aScore >= bScore && p3 > bScore)
            {
                bScore = p3;
                bPolong = 0x04;
                bIsBiggerSide = false;
            }
            else if (aScore < bScore && p3 > aScore)
            {
                aScore = p3;
                aPolong = 0x04;
                aIsBiggerSide = false;
            }

            //Decide if (0,0,0,1) is closer.
            real p4 = 2 - inSum + wins;
            if (aScore >= bScore && p4 > bScore)
            {
                bScore = p4;
                bPolong = 0x08;
                bIsBiggerSide = false;
            }
            else if (aScore < bScore && p4 > aScore)
            {
                aScore = p4;
                aPolong = 0x08;
                aIsBiggerSide = false;
            }

            //Where each of the two closest polongs are determines how the extra three vertices are
            //calculated.
            if (aIsBiggerSide == bIsBiggerSide)
            {
                if (aIsBiggerSide)
                { //Both closest polongs on the bigger side
                    byte c1 = cast(byte)(aPolong | bPolong);
                    byte c2 = cast(byte)(aPolong & bPolong);
                    if ((c1 & 0x01) == 0)
                    {
                        xsv_ext0 = xsb;
                        xsv_ext1 = xsb - 1;
                        dx_ext0 = dx0 - 3 * SQUISH_CONSTANT_4D;
                        dx_ext1 = dx0 + 1 - 2 * SQUISH_CONSTANT_4D;
                    }
                    else
                    {
                        xsv_ext0 = xsv_ext1 = xsb + 1;
                        dx_ext0 = dx0 - 1 - 3 * SQUISH_CONSTANT_4D;
                        dx_ext1 = dx0 - 1 - 2 * SQUISH_CONSTANT_4D;
                    }

                    if ((c1 & 0x02) == 0)
                    {
                        ysv_ext0 = ysb;
                        ysv_ext1 = ysb - 1;
                        dy_ext0 = dy0 - 3 * SQUISH_CONSTANT_4D;
                        dy_ext1 = dy0 + 1 - 2 * SQUISH_CONSTANT_4D;
                    }
                    else
                    {
                        ysv_ext0 = ysv_ext1 = ysb + 1;
                        dy_ext0 = dy0 - 1 - 3 * SQUISH_CONSTANT_4D;
                        dy_ext1 = dy0 - 1 - 2 * SQUISH_CONSTANT_4D;
                    }

                    if ((c1 & 0x04) == 0)
                    {
                        zsv_ext0 = zsb;
                        zsv_ext1 = zsb - 1;
                        dz_ext0 = dz0 - 3 * SQUISH_CONSTANT_4D;
                        dz_ext1 = dz0 + 1 - 2 * SQUISH_CONSTANT_4D;
                    }
                    else
                    {
                        zsv_ext0 = zsv_ext1 = zsb + 1;
                        dz_ext0 = dz0 - 1 - 3 * SQUISH_CONSTANT_4D;
                        dz_ext1 = dz0 - 1 - 2 * SQUISH_CONSTANT_4D;
                    }

                    if ((c1 & 0x08) == 0)
                    {
                        wsv_ext0 = wsb;
                        wsv_ext1 = wsb - 1;
                        dw_ext0 = dw0 - 3 * SQUISH_CONSTANT_4D;
                        dw_ext1 = dw0 + 1 - 2 * SQUISH_CONSTANT_4D;
                    }
                    else
                    {
                        wsv_ext0 = wsv_ext1 = wsb + 1;
                        dw_ext0 = dw0 - 1 - 3 * SQUISH_CONSTANT_4D;
                        dw_ext1 = dw0 - 1 - 2 * SQUISH_CONSTANT_4D;
                    }

                    //One combination is a permutation of (0,0,0,2) based on c2
                    xsv_ext2 = xsb;
                    ysv_ext2 = ysb;
                    zsv_ext2 = zsb;
                    wsv_ext2 = wsb;
                    dx_ext2 = dx0 - 2 * SQUISH_CONSTANT_4D;
                    dy_ext2 = dy0 - 2 * SQUISH_CONSTANT_4D;
                    dz_ext2 = dz0 - 2 * SQUISH_CONSTANT_4D;
                    dw_ext2 = dw0 - 2 * SQUISH_CONSTANT_4D;
                    if ((c2 & 0x01) != 0)
                    {
                        xsv_ext2 += 2;
                        dx_ext2 -= 2;
                    }
                    else if ((c2 & 0x02) != 0)
                    {
                        ysv_ext2 += 2;
                        dy_ext2 -= 2;
                    }
                    else if ((c2 & 0x04) != 0)
                    {
                        zsv_ext2 += 2;
                        dz_ext2 -= 2;
                    }
                    else
                    {
                        wsv_ext2 += 2;
                        dw_ext2 -= 2;
                    }

                }
                else
                { //Both closest polongs on the smaller side
                    //One of the two extra polongs is (0,0,0,0)
                    xsv_ext2 = xsb;
                    ysv_ext2 = ysb;
                    zsv_ext2 = zsb;
                    wsv_ext2 = wsb;
                    dx_ext2 = dx0;
                    dy_ext2 = dy0;
                    dz_ext2 = dz0;
                    dw_ext2 = dw0;

                    //Other two polongs are based on the omitted axes.
                    byte c = cast(byte)(aPolong | bPolong);

                    if ((c & 0x01) == 0)
                    {
                        xsv_ext0 = xsb - 1;
                        xsv_ext1 = xsb;
                        dx_ext0 = dx0 + 1 - SQUISH_CONSTANT_4D;
                        dx_ext1 = dx0 - SQUISH_CONSTANT_4D;
                    }
                    else
                    {
                        xsv_ext0 = xsv_ext1 = xsb + 1;
                        dx_ext0 = dx_ext1 = dx0 - 1 - SQUISH_CONSTANT_4D;
                    }

                    if ((c & 0x02) == 0)
                    {
                        ysv_ext0 = ysv_ext1 = ysb;
                        dy_ext0 = dy_ext1 = dy0 - SQUISH_CONSTANT_4D;
                        if ((c & 0x01) == 0x01)
                        {
                            ysv_ext0 -= 1;
                            dy_ext0 += 1;
                        }
                        else
                        {
                            ysv_ext1 -= 1;
                            dy_ext1 += 1;
                        }
                    }
                    else
                    {
                        ysv_ext0 = ysv_ext1 = ysb + 1;
                        dy_ext0 = dy_ext1 = dy0 - 1 - SQUISH_CONSTANT_4D;
                    }

                    if ((c & 0x04) == 0)
                    {
                        zsv_ext0 = zsv_ext1 = zsb;
                        dz_ext0 = dz_ext1 = dz0 - SQUISH_CONSTANT_4D;
                        if ((c & 0x03) == 0x03)
                        {
                            zsv_ext0 -= 1;
                            dz_ext0 += 1;
                        }
                        else
                        {
                            zsv_ext1 -= 1;
                            dz_ext1 += 1;
                        }
                    }
                    else
                    {
                        zsv_ext0 = zsv_ext1 = zsb + 1;
                        dz_ext0 = dz_ext1 = dz0 - 1 - SQUISH_CONSTANT_4D;
                    }

                    if ((c & 0x08) == 0)
                    {
                        wsv_ext0 = wsb;
                        wsv_ext1 = wsb - 1;
                        dw_ext0 = dw0 - SQUISH_CONSTANT_4D;
                        dw_ext1 = dw0 + 1 - SQUISH_CONSTANT_4D;
                    }
                    else
                    {
                        wsv_ext0 = wsv_ext1 = wsb + 1;
                        dw_ext0 = dw_ext1 = dw0 - 1 - SQUISH_CONSTANT_4D;
                    }

                }
            }
            else
            { //One polong on each "side"
                byte c1, c2;
                if (aIsBiggerSide)
                {
                    c1 = aPolong;
                    c2 = bPolong;
                }
                else
                {
                    c1 = bPolong;
                    c2 = aPolong;
                }

                //Two contributions are the bigger-sided polong with each 0 replaced with -1.
                if ((c1 & 0x01) == 0)
                {
                    xsv_ext0 = xsb - 1;
                    xsv_ext1 = xsb;
                    dx_ext0 = dx0 + 1 - SQUISH_CONSTANT_4D;
                    dx_ext1 = dx0 - SQUISH_CONSTANT_4D;
                }
                else
                {
                    xsv_ext0 = xsv_ext1 = xsb + 1;
                    dx_ext0 = dx_ext1 = dx0 - 1 - SQUISH_CONSTANT_4D;
                }

                if ((c1 & 0x02) == 0)
                {
                    ysv_ext0 = ysv_ext1 = ysb;
                    dy_ext0 = dy_ext1 = dy0 - SQUISH_CONSTANT_4D;
                    if ((c1 & 0x01) == 0x01)
                    {
                        ysv_ext0 -= 1;
                        dy_ext0 += 1;
                    }
                    else
                    {
                        ysv_ext1 -= 1;
                        dy_ext1 += 1;
                    }
                }
                else
                {
                    ysv_ext0 = ysv_ext1 = ysb + 1;
                    dy_ext0 = dy_ext1 = dy0 - 1 - SQUISH_CONSTANT_4D;
                }

                if ((c1 & 0x04) == 0)
                {
                    zsv_ext0 = zsv_ext1 = zsb;
                    dz_ext0 = dz_ext1 = dz0 - SQUISH_CONSTANT_4D;
                    if ((c1 & 0x03) == 0x03)
                    {
                        zsv_ext0 -= 1;
                        dz_ext0 += 1;
                    }
                    else
                    {
                        zsv_ext1 -= 1;
                        dz_ext1 += 1;
                    }
                }
                else
                {
                    zsv_ext0 = zsv_ext1 = zsb + 1;
                    dz_ext0 = dz_ext1 = dz0 - 1 - SQUISH_CONSTANT_4D;
                }

                if ((c1 & 0x08) == 0)
                {
                    wsv_ext0 = wsb;
                    wsv_ext1 = wsb - 1;
                    dw_ext0 = dw0 - SQUISH_CONSTANT_4D;
                    dw_ext1 = dw0 + 1 - SQUISH_CONSTANT_4D;
                }
                else
                {
                    wsv_ext0 = wsv_ext1 = wsb + 1;
                    dw_ext0 = dw_ext1 = dw0 - 1 - SQUISH_CONSTANT_4D;
                }

                //One contribution is a permutation of (0,0,0,2) based on the smaller-sided polong
                xsv_ext2 = xsb;
                ysv_ext2 = ysb;
                zsv_ext2 = zsb;
                wsv_ext2 = wsb;
                dx_ext2 = dx0 - 2 * SQUISH_CONSTANT_4D;
                dy_ext2 = dy0 - 2 * SQUISH_CONSTANT_4D;
                dz_ext2 = dz0 - 2 * SQUISH_CONSTANT_4D;
                dw_ext2 = dw0 - 2 * SQUISH_CONSTANT_4D;
                if ((c2 & 0x01) != 0)
                {
                    xsv_ext2 += 2;
                    dx_ext2 -= 2;
                }
                else if ((c2 & 0x02) != 0)
                {
                    ysv_ext2 += 2;
                    dy_ext2 -= 2;
                }
                else if ((c2 & 0x04) != 0)
                {
                    zsv_ext2 += 2;
                    dz_ext2 -= 2;
                }
                else
                {
                    wsv_ext2 += 2;
                    dw_ext2 -= 2;
                }
            }

            //Contribution (1,0,0,0)
            real dx1 = dx0 - 1 - SQUISH_CONSTANT_4D;
            real dy1 = dy0 - 0 - SQUISH_CONSTANT_4D;
            real dz1 = dz0 - 0 - SQUISH_CONSTANT_4D;
            real dw1 = dw0 - 0 - SQUISH_CONSTANT_4D;
            real attn1 = 2 - dx1 * dx1 - dy1 * dy1 - dz1 * dz1 - dw1 * dw1;
            if (attn1 > 0)
            {
                attn1 *= attn1;
                value += attn1 * attn1 * extrapolate(xsb + 1, ysb + 0, zsb + 0,
                        wsb + 0, dx1, dy1, dz1, dw1);
            }

            //Contribution (0,1,0,0)
            real dx2 = dx0 - 0 - SQUISH_CONSTANT_4D;
            real dy2 = dy0 - 1 - SQUISH_CONSTANT_4D;
            real dz2 = dz1;
            real dw2 = dw1;
            real attn2 = 2 - dx2 * dx2 - dy2 * dy2 - dz2 * dz2 - dw2 * dw2;
            if (attn2 > 0)
            {
                attn2 *= attn2;
                value += attn2 * attn2 * extrapolate(xsb + 0, ysb + 1, zsb + 0,
                        wsb + 0, dx2, dy2, dz2, dw2);
            }

            //Contribution (0,0,1,0)
            real dx3 = dx2;
            real dy3 = dy1;
            real dz3 = dz0 - 1 - SQUISH_CONSTANT_4D;
            real dw3 = dw1;
            real attn3 = 2 - dx3 * dx3 - dy3 * dy3 - dz3 * dz3 - dw3 * dw3;
            if (attn3 > 0)
            {
                attn3 *= attn3;
                value += attn3 * attn3 * extrapolate(xsb + 0, ysb + 0, zsb + 1,
                        wsb + 0, dx3, dy3, dz3, dw3);
            }

            //Contribution (0,0,0,1)
            real dx4 = dx2;
            real dy4 = dy1;
            real dz4 = dz1;
            real dw4 = dw0 - 1 - SQUISH_CONSTANT_4D;
            real attn4 = 2 - dx4 * dx4 - dy4 * dy4 - dz4 * dz4 - dw4 * dw4;
            if (attn4 > 0)
            {
                attn4 *= attn4;
                value += attn4 * attn4 * extrapolate(xsb + 0, ysb + 0, zsb + 0,
                        wsb + 1, dx4, dy4, dz4, dw4);
            }

            //Contribution (1,1,0,0)
            real dx5 = dx0 - 1 - 2 * SQUISH_CONSTANT_4D;
            real dy5 = dy0 - 1 - 2 * SQUISH_CONSTANT_4D;
            real dz5 = dz0 - 0 - 2 * SQUISH_CONSTANT_4D;
            real dw5 = dw0 - 0 - 2 * SQUISH_CONSTANT_4D;
            real attn5 = 2 - dx5 * dx5 - dy5 * dy5 - dz5 * dz5 - dw5 * dw5;
            if (attn5 > 0)
            {
                attn5 *= attn5;
                value += attn5 * attn5 * extrapolate(xsb + 1, ysb + 1, zsb + 0,
                        wsb + 0, dx5, dy5, dz5, dw5);
            }

            //Contribution (1,0,1,0)
            real dx6 = dx0 - 1 - 2 * SQUISH_CONSTANT_4D;
            real dy6 = dy0 - 0 - 2 * SQUISH_CONSTANT_4D;
            real dz6 = dz0 - 1 - 2 * SQUISH_CONSTANT_4D;
            real dw6 = dw0 - 0 - 2 * SQUISH_CONSTANT_4D;
            real attn6 = 2 - dx6 * dx6 - dy6 * dy6 - dz6 * dz6 - dw6 * dw6;
            if (attn6 > 0)
            {
                attn6 *= attn6;
                value += attn6 * attn6 * extrapolate(xsb + 1, ysb + 0, zsb + 1,
                        wsb + 0, dx6, dy6, dz6, dw6);
            }

            //Contribution (1,0,0,1)
            real dx7 = dx0 - 1 - 2 * SQUISH_CONSTANT_4D;
            real dy7 = dy0 - 0 - 2 * SQUISH_CONSTANT_4D;
            real dz7 = dz0 - 0 - 2 * SQUISH_CONSTANT_4D;
            real dw7 = dw0 - 1 - 2 * SQUISH_CONSTANT_4D;
            real attn7 = 2 - dx7 * dx7 - dy7 * dy7 - dz7 * dz7 - dw7 * dw7;
            if (attn7 > 0)
            {
                attn7 *= attn7;
                value += attn7 * attn7 * extrapolate(xsb + 1, ysb + 0, zsb + 0,
                        wsb + 1, dx7, dy7, dz7, dw7);
            }

            //Contribution (0,1,1,0)
            real dx8 = dx0 - 0 - 2 * SQUISH_CONSTANT_4D;
            real dy8 = dy0 - 1 - 2 * SQUISH_CONSTANT_4D;
            real dz8 = dz0 - 1 - 2 * SQUISH_CONSTANT_4D;
            real dw8 = dw0 - 0 - 2 * SQUISH_CONSTANT_4D;
            real attn8 = 2 - dx8 * dx8 - dy8 * dy8 - dz8 * dz8 - dw8 * dw8;
            if (attn8 > 0)
            {
                attn8 *= attn8;
                value += attn8 * attn8 * extrapolate(xsb + 0, ysb + 1, zsb + 1,
                        wsb + 0, dx8, dy8, dz8, dw8);
            }

            //Contribution (0,1,0,1)
            real dx9 = dx0 - 0 - 2 * SQUISH_CONSTANT_4D;
            real dy9 = dy0 - 1 - 2 * SQUISH_CONSTANT_4D;
            real dz9 = dz0 - 0 - 2 * SQUISH_CONSTANT_4D;
            real dw9 = dw0 - 1 - 2 * SQUISH_CONSTANT_4D;
            real attn9 = 2 - dx9 * dx9 - dy9 * dy9 - dz9 * dz9 - dw9 * dw9;
            if (attn9 > 0)
            {
                attn9 *= attn9;
                value += attn9 * attn9 * extrapolate(xsb + 0, ysb + 1, zsb + 0,
                        wsb + 1, dx9, dy9, dz9, dw9);
            }

            //Contribution (0,0,1,1)
            real dx10 = dx0 - 0 - 2 * SQUISH_CONSTANT_4D;
            real dy10 = dy0 - 0 - 2 * SQUISH_CONSTANT_4D;
            real dz10 = dz0 - 1 - 2 * SQUISH_CONSTANT_4D;
            real dw10 = dw0 - 1 - 2 * SQUISH_CONSTANT_4D;
            real attn10 = 2 - dx10 * dx10 - dy10 * dy10 - dz10 * dz10 - dw10 * dw10;
            if (attn10 > 0)
            {
                attn10 *= attn10;
                value += attn10 * attn10 * extrapolate(xsb + 0, ysb + 0, zsb + 1,
                        wsb + 1, dx10, dy10, dz10, dw10);
            }
        }
        else
        { //We're inside the second dispentachoron (Rectified 4-Simplex)
            real aScore;
            byte aPolong;
            bool aIsBiggerSide = true;
            real bScore;
            byte bPolong;
            bool bIsBiggerSide = true;

            //Decide between (0,0,1,1) and (1,1,0,0)
            if (xins + yins < zins + wins)
            {
                aScore = xins + yins;
                aPolong = 0x0C;
            }
            else
            {
                aScore = zins + wins;
                aPolong = 0x03;
            }

            //Decide between (0,1,0,1) and (1,0,1,0)
            if (xins + zins < yins + wins)
            {
                bScore = xins + zins;
                bPolong = 0x0A;
            }
            else
            {
                bScore = yins + wins;
                bPolong = 0x05;
            }

            //Closer between (0,1,1,0) and (1,0,0,1) will replace the further of a and b, if closer.
            if (xins + wins < yins + zins)
            {
                real score = xins + wins;
                if (aScore <= bScore && score < bScore)
                {
                    bScore = score;
                    bPolong = 0x06;
                }
                else if (aScore > bScore && score < aScore)
                {
                    aScore = score;
                    aPolong = 0x06;
                }
            }
            else
            {
                real score = yins + zins;
                if (aScore <= bScore && score < bScore)
                {
                    bScore = score;
                    bPolong = 0x09;
                }
                else if (aScore > bScore && score < aScore)
                {
                    aScore = score;
                    aPolong = 0x09;
                }
            }

            //Decide if (0,1,1,1) is closer.
            real p1 = 3 - inSum + xins;
            if (aScore <= bScore && p1 < bScore)
            {
                bScore = p1;
                bPolong = 0x0E;
                bIsBiggerSide = false;
            }
            else if (aScore > bScore && p1 < aScore)
            {
                aScore = p1;
                aPolong = 0x0E;
                aIsBiggerSide = false;
            }

            //Decide if (1,0,1,1) is closer.
            real p2 = 3 - inSum + yins;
            if (aScore <= bScore && p2 < bScore)
            {
                bScore = p2;
                bPolong = 0x0D;
                bIsBiggerSide = false;
            }
            else if (aScore > bScore && p2 < aScore)
            {
                aScore = p2;
                aPolong = 0x0D;
                aIsBiggerSide = false;
            }

            //Decide if (1,1,0,1) is closer.
            real p3 = 3 - inSum + zins;
            if (aScore <= bScore && p3 < bScore)
            {
                bScore = p3;
                bPolong = 0x0B;
                bIsBiggerSide = false;
            }
            else if (aScore > bScore && p3 < aScore)
            {
                aScore = p3;
                aPolong = 0x0B;
                aIsBiggerSide = false;
            }

            //Decide if (1,1,1,0) is closer.
            real p4 = 3 - inSum + wins;
            if (aScore <= bScore && p4 < bScore)
            {
                bScore = p4;
                bPolong = 0x07;
                bIsBiggerSide = false;
            }
            else if (aScore > bScore && p4 < aScore)
            {
                aScore = p4;
                aPolong = 0x07;
                aIsBiggerSide = false;
            }

            //Where each of the two closest polongs are determines how the extra three vertices are
            //calculated.
            if (aIsBiggerSide == bIsBiggerSide)
            {
                if (aIsBiggerSide)
                { //Both closest polongs on the bigger side
                    byte c1 = cast(byte)(aPolong & bPolong);
                    byte c2 = cast(byte)(aPolong | bPolong);

                    //Two contributions are permutations of (0,0,0,1) and (0,0,0,2) based on c1
                    xsv_ext0 = xsv_ext1 = xsb;
                    ysv_ext0 = ysv_ext1 = ysb;
                    zsv_ext0 = zsv_ext1 = zsb;
                    wsv_ext0 = wsv_ext1 = wsb;
                    dx_ext0 = dx0 - SQUISH_CONSTANT_4D;
                    dy_ext0 = dy0 - SQUISH_CONSTANT_4D;
                    dz_ext0 = dz0 - SQUISH_CONSTANT_4D;
                    dw_ext0 = dw0 - SQUISH_CONSTANT_4D;
                    dx_ext1 = dx0 - 2 * SQUISH_CONSTANT_4D;
                    dy_ext1 = dy0 - 2 * SQUISH_CONSTANT_4D;
                    dz_ext1 = dz0 - 2 * SQUISH_CONSTANT_4D;
                    dw_ext1 = dw0 - 2 * SQUISH_CONSTANT_4D;
                    if ((c1 & 0x01) != 0)
                    {
                        xsv_ext0 += 1;
                        dx_ext0 -= 1;
                        xsv_ext1 += 2;
                        dx_ext1 -= 2;
                    }
                    else if ((c1 & 0x02) != 0)
                    {
                        ysv_ext0 += 1;
                        dy_ext0 -= 1;
                        ysv_ext1 += 2;
                        dy_ext1 -= 2;
                    }
                    else if ((c1 & 0x04) != 0)
                    {
                        zsv_ext0 += 1;
                        dz_ext0 -= 1;
                        zsv_ext1 += 2;
                        dz_ext1 -= 2;
                    }
                    else
                    {
                        wsv_ext0 += 1;
                        dw_ext0 -= 1;
                        wsv_ext1 += 2;
                        dw_ext1 -= 2;
                    }

                    //One contribution is a permutation of (1,1,1,-1) based on c2
                    xsv_ext2 = xsb + 1;
                    ysv_ext2 = ysb + 1;
                    zsv_ext2 = zsb + 1;
                    wsv_ext2 = wsb + 1;
                    dx_ext2 = dx0 - 1 - 2 * SQUISH_CONSTANT_4D;
                    dy_ext2 = dy0 - 1 - 2 * SQUISH_CONSTANT_4D;
                    dz_ext2 = dz0 - 1 - 2 * SQUISH_CONSTANT_4D;
                    dw_ext2 = dw0 - 1 - 2 * SQUISH_CONSTANT_4D;
                    if ((c2 & 0x01) == 0)
                    {
                        xsv_ext2 -= 2;
                        dx_ext2 += 2;
                    }
                    else if ((c2 & 0x02) == 0)
                    {
                        ysv_ext2 -= 2;
                        dy_ext2 += 2;
                    }
                    else if ((c2 & 0x04) == 0)
                    {
                        zsv_ext2 -= 2;
                        dz_ext2 += 2;
                    }
                    else
                    {
                        wsv_ext2 -= 2;
                        dw_ext2 += 2;
                    }
                }
                else
                { //Both closest polongs on the smaller side
                    //One of the two extra polongs is (1,1,1,1)
                    xsv_ext2 = xsb + 1;
                    ysv_ext2 = ysb + 1;
                    zsv_ext2 = zsb + 1;
                    wsv_ext2 = wsb + 1;
                    dx_ext2 = dx0 - 1 - 4 * SQUISH_CONSTANT_4D;
                    dy_ext2 = dy0 - 1 - 4 * SQUISH_CONSTANT_4D;
                    dz_ext2 = dz0 - 1 - 4 * SQUISH_CONSTANT_4D;
                    dw_ext2 = dw0 - 1 - 4 * SQUISH_CONSTANT_4D;

                    //Other two polongs are based on the shared axes.
                    byte c = cast(byte)(aPolong & bPolong);

                    if ((c & 0x01) != 0)
                    {
                        xsv_ext0 = xsb + 2;
                        xsv_ext1 = xsb + 1;
                        dx_ext0 = dx0 - 2 - 3 * SQUISH_CONSTANT_4D;
                        dx_ext1 = dx0 - 1 - 3 * SQUISH_CONSTANT_4D;
                    }
                    else
                    {
                        xsv_ext0 = xsv_ext1 = xsb;
                        dx_ext0 = dx_ext1 = dx0 - 3 * SQUISH_CONSTANT_4D;
                    }

                    if ((c & 0x02) != 0)
                    {
                        ysv_ext0 = ysv_ext1 = ysb + 1;
                        dy_ext0 = dy_ext1 = dy0 - 1 - 3 * SQUISH_CONSTANT_4D;
                        if ((c & 0x01) == 0)
                        {
                            ysv_ext0 += 1;
                            dy_ext0 -= 1;
                        }
                        else
                        {
                            ysv_ext1 += 1;
                            dy_ext1 -= 1;
                        }
                    }
                    else
                    {
                        ysv_ext0 = ysv_ext1 = ysb;
                        dy_ext0 = dy_ext1 = dy0 - 3 * SQUISH_CONSTANT_4D;
                    }

                    if ((c & 0x04) != 0)
                    {
                        zsv_ext0 = zsv_ext1 = zsb + 1;
                        dz_ext0 = dz_ext1 = dz0 - 1 - 3 * SQUISH_CONSTANT_4D;
                        if ((c & 0x03) == 0)
                        {
                            zsv_ext0 += 1;
                            dz_ext0 -= 1;
                        }
                        else
                        {
                            zsv_ext1 += 1;
                            dz_ext1 -= 1;
                        }
                    }
                    else
                    {
                        zsv_ext0 = zsv_ext1 = zsb;
                        dz_ext0 = dz_ext1 = dz0 - 3 * SQUISH_CONSTANT_4D;
                    }

                    if ((c & 0x08) != 0)
                    {
                        wsv_ext0 = wsb + 1;
                        wsv_ext1 = wsb + 2;
                        dw_ext0 = dw0 - 1 - 3 * SQUISH_CONSTANT_4D;
                        dw_ext1 = dw0 - 2 - 3 * SQUISH_CONSTANT_4D;
                    }
                    else
                    {
                        wsv_ext0 = wsv_ext1 = wsb;
                        dw_ext0 = dw_ext1 = dw0 - 3 * SQUISH_CONSTANT_4D;
                    }
                }
            }
            else
            { //One polong on each "side"
                byte c1, c2;
                if (aIsBiggerSide)
                {
                    c1 = aPolong;
                    c2 = bPolong;
                }
                else
                {
                    c1 = bPolong;
                    c2 = aPolong;
                }

                //Two contributions are the bigger-sided polong with each 1 replaced with 2.
                if ((c1 & 0x01) != 0)
                {
                    xsv_ext0 = xsb + 2;
                    xsv_ext1 = xsb + 1;
                    dx_ext0 = dx0 - 2 - 3 * SQUISH_CONSTANT_4D;
                    dx_ext1 = dx0 - 1 - 3 * SQUISH_CONSTANT_4D;
                }
                else
                {
                    xsv_ext0 = xsv_ext1 = xsb;
                    dx_ext0 = dx_ext1 = dx0 - 3 * SQUISH_CONSTANT_4D;
                }

                if ((c1 & 0x02) != 0)
                {
                    ysv_ext0 = ysv_ext1 = ysb + 1;
                    dy_ext0 = dy_ext1 = dy0 - 1 - 3 * SQUISH_CONSTANT_4D;
                    if ((c1 & 0x01) == 0)
                    {
                        ysv_ext0 += 1;
                        dy_ext0 -= 1;
                    }
                    else
                    {
                        ysv_ext1 += 1;
                        dy_ext1 -= 1;
                    }
                }
                else
                {
                    ysv_ext0 = ysv_ext1 = ysb;
                    dy_ext0 = dy_ext1 = dy0 - 3 * SQUISH_CONSTANT_4D;
                }

                if ((c1 & 0x04) != 0)
                {
                    zsv_ext0 = zsv_ext1 = zsb + 1;
                    dz_ext0 = dz_ext1 = dz0 - 1 - 3 * SQUISH_CONSTANT_4D;
                    if ((c1 & 0x03) == 0)
                    {
                        zsv_ext0 += 1;
                        dz_ext0 -= 1;
                    }
                    else
                    {
                        zsv_ext1 += 1;
                        dz_ext1 -= 1;
                    }
                }
                else
                {
                    zsv_ext0 = zsv_ext1 = zsb;
                    dz_ext0 = dz_ext1 = dz0 - 3 * SQUISH_CONSTANT_4D;
                }

                if ((c1 & 0x08) != 0)
                {
                    wsv_ext0 = wsb + 1;
                    wsv_ext1 = wsb + 2;
                    dw_ext0 = dw0 - 1 - 3 * SQUISH_CONSTANT_4D;
                    dw_ext1 = dw0 - 2 - 3 * SQUISH_CONSTANT_4D;
                }
                else
                {
                    wsv_ext0 = wsv_ext1 = wsb;
                    dw_ext0 = dw_ext1 = dw0 - 3 * SQUISH_CONSTANT_4D;
                }

                //One contribution is a permutation of (1,1,1,-1) based on the smaller-sided polong
                xsv_ext2 = xsb + 1;
                ysv_ext2 = ysb + 1;
                zsv_ext2 = zsb + 1;
                wsv_ext2 = wsb + 1;
                dx_ext2 = dx0 - 1 - 2 * SQUISH_CONSTANT_4D;
                dy_ext2 = dy0 - 1 - 2 * SQUISH_CONSTANT_4D;
                dz_ext2 = dz0 - 1 - 2 * SQUISH_CONSTANT_4D;
                dw_ext2 = dw0 - 1 - 2 * SQUISH_CONSTANT_4D;
                if ((c2 & 0x01) == 0)
                {
                    xsv_ext2 -= 2;
                    dx_ext2 += 2;
                }
                else if ((c2 & 0x02) == 0)
                {
                    ysv_ext2 -= 2;
                    dy_ext2 += 2;
                }
                else if ((c2 & 0x04) == 0)
                {
                    zsv_ext2 -= 2;
                    dz_ext2 += 2;
                }
                else
                {
                    wsv_ext2 -= 2;
                    dw_ext2 += 2;
                }
            }

            //Contribution (1,1,1,0)
            real dx4 = dx0 - 1 - 3 * SQUISH_CONSTANT_4D;
            real dy4 = dy0 - 1 - 3 * SQUISH_CONSTANT_4D;
            real dz4 = dz0 - 1 - 3 * SQUISH_CONSTANT_4D;
            real dw4 = dw0 - 3 * SQUISH_CONSTANT_4D;
            real attn4 = 2 - dx4 * dx4 - dy4 * dy4 - dz4 * dz4 - dw4 * dw4;
            if (attn4 > 0)
            {
                attn4 *= attn4;
                value += attn4 * attn4 * extrapolate(xsb + 1, ysb + 1, zsb + 1,
                        wsb + 0, dx4, dy4, dz4, dw4);
            }

            //Contribution (1,1,0,1)
            real dx3 = dx4;
            real dy3 = dy4;
            real dz3 = dz0 - 3 * SQUISH_CONSTANT_4D;
            real dw3 = dw0 - 1 - 3 * SQUISH_CONSTANT_4D;
            real attn3 = 2 - dx3 * dx3 - dy3 * dy3 - dz3 * dz3 - dw3 * dw3;
            if (attn3 > 0)
            {
                attn3 *= attn3;
                value += attn3 * attn3 * extrapolate(xsb + 1, ysb + 1, zsb + 0,
                        wsb + 1, dx3, dy3, dz3, dw3);
            }

            //Contribution (1,0,1,1)
            real dx2 = dx4;
            real dy2 = dy0 - 3 * SQUISH_CONSTANT_4D;
            real dz2 = dz4;
            real dw2 = dw3;
            real attn2 = 2 - dx2 * dx2 - dy2 * dy2 - dz2 * dz2 - dw2 * dw2;
            if (attn2 > 0)
            {
                attn2 *= attn2;
                value += attn2 * attn2 * extrapolate(xsb + 1, ysb + 0, zsb + 1,
                        wsb + 1, dx2, dy2, dz2, dw2);
            }

            //Contribution (0,1,1,1)
            real dx1 = dx0 - 3 * SQUISH_CONSTANT_4D;
            real dz1 = dz4;
            real dy1 = dy4;
            real dw1 = dw3;
            real attn1 = 2 - dx1 * dx1 - dy1 * dy1 - dz1 * dz1 - dw1 * dw1;
            if (attn1 > 0)
            {
                attn1 *= attn1;
                value += attn1 * attn1 * extrapolate(xsb + 0, ysb + 1, zsb + 1,
                        wsb + 1, dx1, dy1, dz1, dw1);
            }

            //Contribution (1,1,0,0)
            real dx5 = dx0 - 1 - 2 * SQUISH_CONSTANT_4D;
            real dy5 = dy0 - 1 - 2 * SQUISH_CONSTANT_4D;
            real dz5 = dz0 - 0 - 2 * SQUISH_CONSTANT_4D;
            real dw5 = dw0 - 0 - 2 * SQUISH_CONSTANT_4D;
            real attn5 = 2 - dx5 * dx5 - dy5 * dy5 - dz5 * dz5 - dw5 * dw5;
            if (attn5 > 0)
            {
                attn5 *= attn5;
                value += attn5 * attn5 * extrapolate(xsb + 1, ysb + 1, zsb + 0,
                        wsb + 0, dx5, dy5, dz5, dw5);
            }

            //Contribution (1,0,1,0)
            real dx6 = dx0 - 1 - 2 * SQUISH_CONSTANT_4D;
            real dy6 = dy0 - 0 - 2 * SQUISH_CONSTANT_4D;
            real dz6 = dz0 - 1 - 2 * SQUISH_CONSTANT_4D;
            real dw6 = dw0 - 0 - 2 * SQUISH_CONSTANT_4D;
            real attn6 = 2 - dx6 * dx6 - dy6 * dy6 - dz6 * dz6 - dw6 * dw6;
            if (attn6 > 0)
            {
                attn6 *= attn6;
                value += attn6 * attn6 * extrapolate(xsb + 1, ysb + 0, zsb + 1,
                        wsb + 0, dx6, dy6, dz6, dw6);
            }

            //Contribution (1,0,0,1)
            real dx7 = dx0 - 1 - 2 * SQUISH_CONSTANT_4D;
            real dy7 = dy0 - 0 - 2 * SQUISH_CONSTANT_4D;
            real dz7 = dz0 - 0 - 2 * SQUISH_CONSTANT_4D;
            real dw7 = dw0 - 1 - 2 * SQUISH_CONSTANT_4D;
            real attn7 = 2 - dx7 * dx7 - dy7 * dy7 - dz7 * dz7 - dw7 * dw7;
            if (attn7 > 0)
            {
                attn7 *= attn7;
                value += attn7 * attn7 * extrapolate(xsb + 1, ysb + 0, zsb + 0,
                        wsb + 1, dx7, dy7, dz7, dw7);
            }

            //Contribution (0,1,1,0)
            real dx8 = dx0 - 0 - 2 * SQUISH_CONSTANT_4D;
            real dy8 = dy0 - 1 - 2 * SQUISH_CONSTANT_4D;
            real dz8 = dz0 - 1 - 2 * SQUISH_CONSTANT_4D;
            real dw8 = dw0 - 0 - 2 * SQUISH_CONSTANT_4D;
            real attn8 = 2 - dx8 * dx8 - dy8 * dy8 - dz8 * dz8 - dw8 * dw8;
            if (attn8 > 0)
            {
                attn8 *= attn8;
                value += attn8 * attn8 * extrapolate(xsb + 0, ysb + 1, zsb + 1,
                        wsb + 0, dx8, dy8, dz8, dw8);
            }

            //Contribution (0,1,0,1)
            real dx9 = dx0 - 0 - 2 * SQUISH_CONSTANT_4D;
            real dy9 = dy0 - 1 - 2 * SQUISH_CONSTANT_4D;
            real dz9 = dz0 - 0 - 2 * SQUISH_CONSTANT_4D;
            real dw9 = dw0 - 1 - 2 * SQUISH_CONSTANT_4D;
            real attn9 = 2 - dx9 * dx9 - dy9 * dy9 - dz9 * dz9 - dw9 * dw9;
            if (attn9 > 0)
            {
                attn9 *= attn9;
                value += attn9 * attn9 * extrapolate(xsb + 0, ysb + 1, zsb + 0,
                        wsb + 1, dx9, dy9, dz9, dw9);
            }

            //Contribution (0,0,1,1)
            real dx10 = dx0 - 0 - 2 * SQUISH_CONSTANT_4D;
            real dy10 = dy0 - 0 - 2 * SQUISH_CONSTANT_4D;
            real dz10 = dz0 - 1 - 2 * SQUISH_CONSTANT_4D;
            real dw10 = dw0 - 1 - 2 * SQUISH_CONSTANT_4D;
            real attn10 = 2 - dx10 * dx10 - dy10 * dy10 - dz10 * dz10 - dw10 * dw10;
            if (attn10 > 0)
            {
                attn10 *= attn10;
                value += attn10 * attn10 * extrapolate(xsb + 0, ysb + 0, zsb + 1,
                        wsb + 1, dx10, dy10, dz10, dw10);
            }
        }

        //First extra vertex
        real attn_ext0 = 2 - dx_ext0 * dx_ext0 - dy_ext0 * dy_ext0 - dz_ext0
            * dz_ext0 - dw_ext0 * dw_ext0;
        if (attn_ext0 > 0)
        {
            attn_ext0 *= attn_ext0;
            value += attn_ext0 * attn_ext0 * extrapolate(xsv_ext0, ysv_ext0,
                    zsv_ext0, wsv_ext0, dx_ext0, dy_ext0, dz_ext0, dw_ext0);
        }

        //Second extra vertex
        real attn_ext1 = 2 - dx_ext1 * dx_ext1 - dy_ext1 * dy_ext1 - dz_ext1
            * dz_ext1 - dw_ext1 * dw_ext1;
        if (attn_ext1 > 0)
        {
            attn_ext1 *= attn_ext1;
            value += attn_ext1 * attn_ext1 * extrapolate(xsv_ext1, ysv_ext1,
                    zsv_ext1, wsv_ext1, dx_ext1, dy_ext1, dz_ext1, dw_ext1);
        }

        //Third extra vertex
        real attn_ext2 = 2 - dx_ext2 * dx_ext2 - dy_ext2 * dy_ext2 - dz_ext2
            * dz_ext2 - dw_ext2 * dw_ext2;
        if (attn_ext2 > 0)
        {
            attn_ext2 *= attn_ext2;
            value += attn_ext2 * attn_ext2 * extrapolate(xsv_ext2, ysv_ext2,
                    zsv_ext2, wsv_ext2, dx_ext2, dy_ext2, dz_ext2, dw_ext2);
        }

        return value / NORM_CONSTANT_4D;
    }

    private real extrapolate(long xsb, long ysb, real dx, real dy)
    {
        long index = perm[(perm[xsb & 0xFF] + ysb) & 0xFF] & 0x0E;
        return gradients2D[index] * dx + gradients2D[index + 1] * dy;
    }

    private real extrapolate(long xsb, long ysb, long zsb, real dx, real dy, real dz)
    {
        long index = permGradIndex3D[(perm[(perm[xsb & 0xFF] + ysb) & 0xFF] + zsb) & 0xFF];
        return gradients3D[index] * dx + gradients3D[index + 1] * dy + gradients3D[index + 2] * dz;
    }

    private real extrapolate(long xsb, long ysb, long zsb, long wsb, real dx,
            real dy, real dz, real dw)
    {
        long index = perm[(perm[(perm[(perm[xsb & 0xFF] + ysb) & 0xFF] + zsb) & 0xFF] + wsb) & 0xFF]
            & 0xFC;
        return gradients4D[index] * dx + gradients4D[index + 1] * dy
            + gradients4D[index + 2] * dz + gradients4D[index + 3] * dw;
    }

    private static long fastFloor(real x)
    {
        long xi = cast(long) x;
        return x < xi ? xi - 1 : xi;
    }

    //Gradients for 2D. They approximate the directions to the
    //vertices of an octagon from the center.
    private static immutable byte[] gradients2D = [
        5, 2, 2, 5, -5, 2, -2, 5, 5, -2, 2, -5, -5, -2, -2, -5,
    ];

    //Gradients for 3D. They approximate the directions to the
    //vertices of a rhombicuboctahedron from the center, skewed so
    //that the triangular and square facets can be inscribed inside
    //circles of the same radius.
    private static immutable byte[] gradients3D = [
        -11, 4, 4, -4, 11, 4, -4, 4, 11, 11, 4, 4, 4, 11, 4, 4, 4, 11, -11,
        -4, 4, -4, -11, 4, -4, -4, 11, 11, -4, 4, 4, -11, 4, 4, -4, 11, -11,
        4, -4, -4, 11, -4, -4, 4, -11, 11, 4, -4, 4, 11, -4, 4, 4, -11,
        -11, -4, -4, -4, -11, -4, -4, -4, -11, 11, -4, -4, 4, -11, -4, 4, -4, -11,
    ];

    //Gradients for 4D. They approximate the directions to the
    //vertices of a disprismatotesseractihexadecachoron from the center,
    //skewed so that the tetrahedral and cubic facets can be inscribed inside
    //spheres of the same radius.
    private static immutable byte[] gradients4D = [
        3, 1, 1, 1, 1, 3, 1, 1, 1, 1, 3, 1, 1, 1, 1, 3, -3, 1, 1, 1, -1, 3,
        1, 1, -1, 1, 3, 1, -1, 1, 1, 3, 3, -1, 1, 1, 1, -3, 1, 1, 1, -1, 3,
        1, 1, -1, 1, 3, -3, -1, 1, 1, -1, -3, 1, 1, -1, -1, 3, 1, -1, -1,
        1, 3, 3, 1, -1, 1, 1, 3, -1, 1, 1, 1, -3, 1, 1, 1, -1, 3, -3, 1,
        -1, 1, -1, 3, -1, 1, -1, 1, -3, 1, -1, 1, -1, 3, 3, -1, -1, 1, 1,
        -3, -1, 1, 1, -1, -3, 1, 1, -1, -1, 3, -3, -1, -1, 1, -1, -3,
        -1, 1, -1, -1, -3, 1, -1, -1, -1, 3, 3, 1, 1, -1, 1, 3, 1, -1, 1,
        1, 3, -1, 1, 1, 1, -3, -3, 1, 1, -1, -1, 3, 1, -1, -1, 1, 3, -1,
        -1, 1, 1, -3, 3, -1, 1, -1, 1, -3, 1, -1, 1, -1, 3, -1, 1, -1, 1,
        -3, -3, -1, 1, -1, -1, -3, 1, -1, -1, -1, 3, -1, -1, -1, 1, -3, 3,
        1, -1, -1, 1, 3, -1, -1, 1, 1, -3, -1, 1, 1, -1, -3, -3, 1, -1,
        -1, -1, 3, -1, -1, -1, 1, -3, -1, -1, 1, -1, -3, 3, -1, -1, -1, 1,
        -3, -1, -1, 1, -1, -3, -1, 1, -1, -1, -3, -3, -1, -1, -1, -1,
        -3, -1, -1, -1, -1, -3, -1, -1, -1, -1, -3,
    ];
}
