///////////////////////////////////////////////////////////////////////////////
///
/// --- Day 3: Squares With Three Sides ---
///
/// Now that you can think clearly, you move deeper into the labyrinth of
/// hallways and office furniture that makes up this part of Easter Bunny HQ.
/// This must be a graphic design department; the walls are covered in
/// specifications for triangles.
///
/// Or are they?
///
/// The design document gives the side lengths of each triangle it describes,
/// but... 5 10 25? Some of these aren't triangles. You can't help but mark
/// the impossible ones.
///
/// In a valid triangle, the sum of any two sides must be larger than the
/// remaining side. For example, the "triangle" given above is impossible,
/// because 5 + 10 is not larger than 25.
///
/// In your puzzle input, how many of the listed triangles are possible?
///
///////////////////////////////////////////////////////////////////////////////

///_* Package declaration ==============================================
package main

///_* Imports ==========================================================
import (
  "os"
  "fmt"
  "bufio"
  "strings"
  "strconv"
)

///_* Code =============================================================
///_ * API -------------------------------------------------------------
func main() {
  os.Exit(run())
}

///_* Internal Functions ===============================================
func solve1(lines [][]string) int {
  validThriangles := 0
  for _, line := range lines {
    a, _ := strconv.Atoi(line[0])
    b, _ := strconv.Atoi(line[1])
    c, _ := strconv.Atoi(line[2])

    if isValidTriangle(a, b, c) {
      validThriangles++
    }
  }

  return validThriangles
}

func solve2(lines [][]string) int {
  validThriangles := 0
  for i := 0; i < len(lines); i += 3 {
    aa, _ := strconv.Atoi(lines[i][0])
    ab, _ := strconv.Atoi(lines[i+1][0])
    ac, _ := strconv.Atoi(lines[i+2][0])
    ba, _ := strconv.Atoi(lines[i][1])
    bb, _ := strconv.Atoi(lines[i+1][1])
    bc, _ := strconv.Atoi(lines[i+2][1])
    ca, _ := strconv.Atoi(lines[i][2])
    cb, _ := strconv.Atoi(lines[i+1][2])
    cc, _ := strconv.Atoi(lines[i+2][2])

    if isValidTriangle(aa, ab, ac) {
      validThriangles++
    }

    if isValidTriangle(ba, bb, bc) {
      validThriangles++
    }

    if isValidTriangle(ca, cb, cc) {
      validThriangles++
    }
  }

  return validThriangles
}

func isValidTriangle(a int, b int, c int) bool {
  if (a+b > c) && (b+c > a) && (a+c > b) {
    return true
  } else {
    return false
  }
}

func run() int {
  if len(os.Args) != 2 {
    fmt.Fprintf(os.Stderr, "%s filename\n", os.Args[0])
    return 1
  }

  file, err := os.Open(os.Args[1])
  if err != nil {
    fmt.Fprintln(os.Stderr, err)
    return 1
  }
  defer file.Close()

  scanner := bufio.NewScanner(file)
  var lines [][]string
  for scanner.Scan() {
    lines = append(lines, strings.Fields(scanner.Text()))
  }

  if err := scanner.Err(); err != nil {
    fmt.Fprintln(os.Stderr, err)
    return 1
  }

  fmt.Println("part1: ", solve1(lines))
  fmt.Println("part2: ", solve2(lines))

  return 0
}

///////////////////////////////////////////////////////////////////////////////

