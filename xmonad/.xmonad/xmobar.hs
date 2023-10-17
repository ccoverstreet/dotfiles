Config {

   -- appearance
   bgColor =      "#323232"
   , fgColor =      "#c8c8c8"
   , position =     Top
   , border =       BottomB
   , borderColor =  "#c8c8c8"
   , font = "xft:Bitstream Vera Sans Mono:size=11:bold:antialias=true"

   -- layout
   , sepChar =  "%"   -- delineator between plugin names and straight text
   , alignSep = "}{"  -- separator between left-right alignment
   , template = "%battery% | %multicpu% | %coretemp% | %memory% | Vol: %volume%}{%UnsafeStdinReader% || %wlo1wi% | %date%  "

   -- general behavior
   , lowerOnStart =     True    -- send to bottom of window stack on start
   , hideOnStart =      False   -- start with window unmapped (hidden)
   , allDesktops =      True    -- show on all desktops
   , overrideRedirect = True    -- set the Override Redirect flag (Xlib)
   , pickBroadest =     False   -- choose widest display (multi-monitor)
   , persistent =       True    -- enable/disable hiding (True = disabled)

   -- plugins
   --   Numbers can be automatically colored according to their value. xmobar
   --   decides color based on a three-tier/two-cutoff system, controlled by
   --   command options:
   --     --Low sets the low cutoff
   --     --High sets the high cutoff
   --
   --     --low sets the color below --Low cutoff
   --     --normal sets the color between --Low and --High cutoffs
   --     --High sets the color above --High cutoff
   --
   --   The --template option controls how the plugin is displayed. Text
   --   color can be set by enclosing in <fc></fc> tags. For more details
   --   see http://projects.haskell.org/xmobar/#system-monitor-plugins.
   , commands =
        [
        -- network activity monitor (dynamic interface resolution)
        --Run DynNetwork     [ "--template" , "<dev>: <tx>kB/s|<rx>kB/s"
        --                     , "--Low"      , "1000"       -- units: kB/s
        --                     , "--High"     , "5000"       -- units: kB/s
        --                     , "--low"      , "darkgreen"
        --                     , "--normal"   , "darkorange"
        --                     , "--high"     , "darkred"
        --                     ] 10

        -- cpu activity monitor
         Run MultiCpu       [ "--template" , "CPU: <autototal>%"
                             , "--Low"      , "50"         -- units: %
                             , "--High"     , "85"         -- units: %
                             , "--low"      , "#709772"
                             , "--normal"   , "#ff9900"
                             , "--high"     , "#be7472"
                             ] 50

        -- cpu core temperature monitor
        , Run CoreTemp       [ "--template" , "Temp: <core0>°C"
                             , "--Low"      , "70"        -- units: °C
                             , "--High"     , "80"        -- units: °C
                             , "--low"      , "#709772"
                             , "--normal"   , "#ff9900"
                             , "--high"     , "#be7472"
                             ] 50

        -- memory usage monitor
        , Run Memory         [ "--template" ,"Mem: <usedratio>%"
                             , "--Low"      , "20"        -- units: %
                             , "--High"     , "90"        -- units: %
                             , "--low"      , "#709772"
                             , "--normal"   , "#ff9900"
                             , "--high"     , "#be7472"
                             ] 50

        -- battery monitor
        , Run Battery        [ "--template" , "Batt: <acstatus>"
                             , "--Low"      , "20"        -- units: %
                             , "--High"     , "80"        -- units: %
                             , "--low"      , "#be7472"
                             , "--normal"   , "#ff9900"
                             , "--high"     , "#709772"

                             , "--" -- battery specific options
                                       -- discharging status
                                       , "-o"	, "<left>% (<timeleft>)"
                                       -- AC "on" status
                                       , "-O"	, "<fc=#ff9900>Charging</fc> <left>%"
                                       -- charged status
                                       , "-i"	, "<fc=#709772>Charged</fc>"
                             ] 50

        , Run Com "jgetvolume" [] "volume" 10
        
		, Run Wireless "wlo1" [] 50

        -- time and date indicator
        --   (%F = y-m-d date, %a = day of week, %T = h:m:s time)
        , Run Date           "<fc=#ABABAB>%F (%a) %T</fc>" "date" 10

        , Run UnsafeStdinReader
        ]
   }
