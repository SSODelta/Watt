foreach $line (<>) {
    $line =~ s/\(\*.*\*\)//;
    print $line;
}
