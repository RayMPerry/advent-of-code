use v6;

sub generate-board($pattern, $repetitions = 1) {
    my @board = [];
    
    for $pattern.split('|') -> $fragment {
        my @subpattern = [];
        @subpattern.push($fragment) for ^$repetitions;
        @board.push(@subpattern.join(''));
    }

    @board;
}

sub calculate-offset(@board, $xcoord, $ycoord) {
    (@board.head.chars * $ycoord) + $xcoord;
}

sub traverse-and-count(@board, $right = 1, $down = 1) {
    my $number-of-trees = 0;
    my $xcoord = 0;
    my $ycoord = 0;

    my $board = @board.join('');
    
    loop {
        my $position = calculate-offset(@board, $xcoord, $ycoord);
        last if $position >= $board.chars;
        $number-of-trees++ if $board.substr($position, 1) ~~ '#';
        $xcoord += $right;
        $ycoord += $down;
    }

    $number-of-trees;
}

DOC CHECK {
    use Test;

    subtest 'Example input 1' => {
        my $pattern = '..##.......|#...#...#..|.#....#..#.|..#.#...#.#|.#...##..#.|..#.##.....|.#.#.#....#|.#........#|#.##...#...|#...##....#|.#..#...#.#|';
        my @board = generate-board($pattern, 3);
        traverse-and-count(@board, 3, 1).&is(7);
    }

    subtest 'Example input 2' => {
        my $pattern = '..##.......|#...#...#..|.#....#..#.|..#.#...#.#|.#...##..#.|..#.##.....|.#.#.#....#|.#........#|#.##...#...|#...##....#|.#..#...#.#|';
        my @board = generate-board($pattern, 10);
        my @offsets = (1, 1), (3, 1), (5, 1), (7, 1), (1, 2);
        my @slopes = ();
        
        for @offsets -> ($right, $down) {
           @slopes.push: traverse-and-count(@board, $right, $down);
        }

        my $answer = [*] @slopes;
        $answer.&is(336);
    }

    subtest 'Day 03 input, Part 1' => {
        my $pattern = 'input.txt'.IO.lines.join('|');
        my @board = generate-board($pattern, 500);
        traverse-and-count(@board, 3, 1).&is(193);
    }

    subtest 'Day 03 input, Part 2' => {
        my $pattern = 'input.txt'.IO.lines.join('|');
        my @board = generate-board($pattern, 500);
        my @offsets = (1, 1), (3, 1), (5, 1), (7, 1), (1, 2);
        my @slopes = ();
        
        for @offsets -> ($right, $down) {
           @slopes.push: traverse-and-count(@board, $right, $down);
        }

        my $answer = [*] @slopes;
        $answer.&is(1355323200);
    }
}
