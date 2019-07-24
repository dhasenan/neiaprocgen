module star.app;

import std.stdio;
import star.galaxy;
import star.planet;

void main(string[] args)
{
    version (linux)
    {
        import etc.linux.memoryerror;
        registerMemoryErrorHandler;
    }
    import std.experimental.logger;
    sharedLog.logLevel = LogLevel.info;
    if (args.length == 1)
    {
        writefln("Usage: %s [galaxy|planet]", args[0]);
        return;
    }
    if (args[1] == "galaxy")
    {
        galaxymain(args[1..$]);
        return;
    }
    if (args[1] == "planet")
    {
        planetmain(args[1..$]);
        return;
    }
    writefln("Usage: %s [galaxy|planet]", args[0]);
}
