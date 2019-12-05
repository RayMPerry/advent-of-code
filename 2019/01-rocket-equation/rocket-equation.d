module rocketequation;

import std.stdio;
import std.math;
import std.conv;
import std.range;
import std.algorithm;

alias calculateFuel = (mass) => floor(mass.to!double / 3) - 2;

double calculateFuelRecursively(ref double sum, double mass) {
  double nextValue = calculateFuel(mass);
  if (nextValue <= 0) return sum;
  sum += nextValue;
  return calculateFuelRecursively(sum, nextValue);
}

void runSimulation() {
  double sum = 0;
  File("input.txt").byLineCopy.each!((number) => calculateFuelRecursively(sum, to!double(number)));
  sum.to!int.writeln;
}

void main() {
  runSimulation();
}

// Local Variables:
// compile-command: "rdmd rocket-equation.d"
// End:
