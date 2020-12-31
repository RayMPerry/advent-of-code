use v6;

grammar PassportValidator {
    rule TOP {
        ^^ <passport-field>+ %% <ws> $$
    }

    token passport-field {
        <birth-year>
        || <issue-year>
        || <expiration-year>
        || <height>
        || <hair-color>
        || <eye-color>
        || <passport-id>
        || <country-id>
    }

    token birth-year {
        'byr:' (19 <[234567890]> \d || 200 <[012]>)
    }

    token issue-year {
        'iyr:' 20 <[12]> \d
    }

    token expiration-year {
        'eyr:' 20 <[23]> \d
    }

    token height {
        'hgt:' (<centimeters> || <inches>)
    }

    token centimeters {
        (1 <[5678]> \d || 19 <[0123]>) 'cm'
    }

    token inches {
        (59 || 6 \d || 7 <[0123456]>) 'in'        
    }
    
    token hair-color {
        'hcl:#' <[0..9a..f]> ** 6
    }

    token eye-color {
        'ecl:' ('amb'||'blu'||'brn'||'gry'||'grn'||'hzl'||'oth')
    }
    
    token passport-id {
        'pid:' \d ** 9
    }

    token country-id {
        'cid:' \S*
    }
}

sub count-valid-passports(@input, $strict = False) {
    my @passports;
    my $passport;
    my $valid-passports = 0;
    
    for @input {
        $passport ~= ' ' ~ $_.trim;
        if not $_.chars {
            @passports.push($passport.trim);
            $passport = '';
        }
        
        LAST { @passports.push($passport.trim); }
    }

    for @passports {
        my $contains-fields = .contains(all(<byr iyr eyr hgt hcl ecl pid>));
        my $fields-valid = so PassportValidator.parse($_);

        $valid-passports++ if $contains-fields;
        $valid-passports-- if $strict && not $fields-valid;
    }
    
    $valid-passports;
}


DOC CHECK {
    use Test;

    subtest 'Example input, Part 1' => {
        my @input = 'example.txt'.IO.lines;
        count-valid-passports(@input).&is(2);
    }

    subtest 'Example input, Part 2' => {
        my @input = 'example2.txt'.IO.lines;
        count-valid-passports(@input, True).&is(4);
    }
    
    subtest 'Day 04, Part 1' => {
        my @input = 'input.txt'.IO.lines;
        count-valid-passports(@input).&is(254);
    }

    subtest 'Day 04, Part 2' => {
        my @input = 'input.txt'.IO.lines;
        count-valid-passports(@input, True).&is(189);
    }
}
