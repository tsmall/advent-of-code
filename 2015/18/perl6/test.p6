sub clear-screen {
    print "\e[2J";
}


sub cursor-home {
    print "\e[H";
}


sub MAIN {
    for 1..10 -> $i {
        clear-screen;
        cursor-home;
        print "\r", '.' x $i;
        print "\n", '.' x $i;
        sleep 1/2;
    }
    print "\n";
}
