module aoc;

import std.stdio;
import std.algorithm;
import std.conv;
import std.exception;
import std.file;
import std.functional;
import std.range;
import std.format;

// Initial iteration:
// currentFrequencies = [0], accumulator = 0, currentItem = (some int)
int logAllFrequencies(ref int[] currentFrequencies, int accumulator, int currentItem) {
  int newFrequency = accumulator + currentItem;
  // TODO: Why isn't this logging the first duplicate?
  if (currentFrequencies.canFind(newFrequency)) {
    "The first duplicate frequency is %s.".format(newFrequency).writeln;
  }

  currentFrequencies ~= newFrequency;
  return newFrequency;
}

void main() {
  int[] currentFrequencies = [0];
  alias logDuplicateFrequencies = partial!(logAllFrequencies, currentFrequencies);

  string fileName = "input.txt";
  enforce(fileName.exists, "The file 'input.txt' doesn't exist.");
  File(fileName, "r")
      .byLine
      .map!(to!int)
      .fold!(logDuplicateFrequencies)
      .writeln;

  currentFrequencies.length.writeln;
}

// Local Variables:
// compile-command: "rdmd chronalCalibration.d"
// End:
