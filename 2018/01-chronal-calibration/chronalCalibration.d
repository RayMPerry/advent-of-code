module aoc;

import std.stdio;
import std.algorithm;
import std.conv;
import std.exception;
import std.file;
import std.functional;
import std.range;
import std.format;

int findDuplicateFrequency(int[] inputs = [0]) {
  int currentFrequency = 0;
  int[] currentFrequencies = [0];

  auto sequence = inputs.cycle;
  while (!sequence.empty) {
    currentFrequency += sequence.front;
    sequence.popFront();
    if (currentFrequencies.canFind(currentFrequency)) {
      "The first duplicate frequency is %s.".format(currentFrequency).writeln;
      break;
    }
    currentFrequencies ~= currentFrequency;
  }

  return currentFrequency;
}

void main() {
  string fileName = "input.txt";
  enforce(fileName.exists, "The file 'input.txt' doesn't exist.");

  int[] fileInputs = [];
  foreach (line; File(fileName, "r").byLine) {
    fileInputs ~= to!int(line);
  }

  findDuplicateFrequency(fileInputs);
}

unittest {
  assert(findDuplicateFrequency() == 0);
  assert(findDuplicateFrequency([+1, -1]) == 0);
  assert(findDuplicateFrequency([+3, +3, +4, -2, -4]) == 10);
  assert(findDuplicateFrequency([-6, +3, +8, +5, -6]) == 5);
  assert(findDuplicateFrequency([+7, +7, -2, -7, -4]) == 14);
}

// Local Variables:
// compile-command: "rdmd chronalCalibration.d"
// End:
