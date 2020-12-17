use v6;

grammar PasswordChecker {
    rule TOP { ^^ <minimum> '-' <maximum> <ws> <letter> ':' <ws> <password> $$ }
    token minimum { <digit>+ }
    token maximum { <digit>+ }
    token letter { <alpha> }
    token password { <alnum>+ }
}

sub check-password-validity(@inputs) {
    my $numberOfValidPasswords = 0;
    for @inputs {
        my $parsedValue = PasswordChecker.parse($_);
        
        my $minimum = $parsedValue<minimum>.Int;
        my $maximum = $parsedValue<maximum>.Int;
        my $letter = $parsedValue<letter>.Str;

        $numberOfValidPasswords++ if $parsedValue<password>.comb.grep(* ~~ $letter).elems ~~ $minimum .. $maximum;
    }
    $numberOfValidPasswords;
}

sub check-password-positions(@inputs) {
    my $numberOfValidPasswords = 0;
    for @inputs {
        my $parsedValue = PasswordChecker.parse($_);

        my $firstPosition = $parsedValue<minimum>.Int;
        my $secondPosition = $parsedValue<maximum>.Int;
        my $letter = $parsedValue<letter>.Str;
        my $password = $parsedValue<password>.Str.comb;

        $numberOfValidPasswords++ if $letter ~~ ($password[$firstPosition - 1], $password[$secondPosition - 1]).one;
    }
    $numberOfValidPasswords;
}

DOC CHECK {
    use Test;

    subtest 'Example input 1' => {
        my @passwords = '1-3 a: abcde', '1-3 b: cdefg', '2-9 c: ccccccccc';
        check-password-validity(@passwords).&is(2);
    }

    subtest 'Example input 2' => {
        my @passwords = '1-3 a: abcde', '1-3 b: cdefg', '2-9 c: ccccccccc';
        check-password-positions(@passwords).&is(1);
    }
    
    subtest 'Day 02 input, Part 1' => {
        my @passwords = 'input.txt'.IO.lines;
        check-password-validity(@passwords).&is(582);
    }

    subtest 'Day 02 input, Part 2' => {
        my @passwords = 'input.txt'.IO.lines;
        check-password-positions(@passwords).&is(729);
    }
}
