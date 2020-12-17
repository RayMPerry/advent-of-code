use v6;

sub find-components(@inputs, $numberOfComponents = 2) {
    return $_ if .sum == 2020 for @inputs.combinations($numberOfComponents);
}

sub multiply-components(@inputs) { [*] @inputs }

DOC CHECK {
    use Test;

    subtest 'Example input 1' => {
        my @inputs = 1721, 979, 366, 299, 675, 1456;
        my @answers = find-components(@inputs);
        @answers.&is((1721, 299));
        multiply-components(@answers).&is(514579);
    }

    subtest 'Example input 2' => {
        my @inputs = 1721, 979, 366, 299, 675, 1456;
        my @answers = find-components(@inputs, 3);
        @answers.&is((979, 366, 675));
        multiply-components(@answers).&is(241861950);
    }

    subtest 'Day 01 input, part 1' => {
        my @inputs = 'input.txt'.IO.lines.map({ .Int });
        my @answers = find-components(@inputs);
        @answers.&is((623, 1397));
        multiply-components(@answers).&is(870331);
    }

    subtest 'Day 01 input, part 2' => {
        my @inputs = 'input.txt'.IO.lines.map({ .Int });
        my @answers = find-components(@inputs, 3);
        @answers.&is((636, 508, 876));
        multiply-components(@answers).&is(283025088);
    }
}
