argv = %1

main:   or %255, argv, 0
        ld %255, %255, 0
        trap 0, 1, 0
        geta %255, string
        trap 0, 1, 0
        pop 0, 0

string: byte ", world", 10, 0
