#!/usr/bin/env php
<?php

define('INPUT_FILE_PATH', __DIR__ . '/../inputs/day06.txt');

define('FLOOR', ord('.'));
define('OBSTACLE', ord('#'));
define('UP', ord('^'));
define('DOWN', ord('v'));
define('LEFT', ord('<'));
define('RIGHT', ord('>'));

define('TURN_90DEG', [
  UP => RIGHT,
  RIGHT => DOWN,
  DOWN => LEFT,
  LEFT => UP,
]);

function parse_grid(string $path = INPUT_FILE_PATH): array
{
  $content = file_get_contents($path);
  return array_map(
    fn($row) => array_map('ord', str_split($row)),
    explode(PHP_EOL, $content),
  );
}

function find_guard(array $grid): array
{
  foreach ($grid as $y => $row) {
    $x = array_search(UP, $row);
    if ($x !== false) {
      return [$y, $x];
    }
  }
}

function next_pos(int $y, int $x, int $dir): array
{
  return match ($dir) {
    UP => [$y - 1, $x],
    DOWN => [$y + 1, $x],
    LEFT => [$y, $x - 1],
    RIGHT => [$y, $x + 1],
  };
}

function is_inside(array $grid, int $y, int $x): bool
{
  return 0 <= $y && $y < count($grid) && 0 <= $x && $x < count($grid[0]);
}

function is_obstacle(array $grid, int $y, int $x): bool
{
  return ($grid[$y][$x] ?? FLOOR) === OBSTACLE;
}

function hash_point(int $y, int $x): int
{
  return ($y << 10) + $x;
}

function count_visited_positions(
  array $grid,
  int $guard_y,
  int $guard_x,
  int $guard_dir = UP,
): int {
  $visited = [];

  do {
    $visited[hash_point($guard_y, $guard_x)] = true;

    list($next_y, $next_x) = next_pos($guard_y, $guard_x, $guard_dir);
    while (is_obstacle($grid, $next_y, $next_x)) {
      $guard_dir = TURN_90DEG[$guard_dir];
      list($next_y, $next_x) = next_pos($guard_y, $guard_x, $guard_dir);
    }

    $guard_x = $next_x;
    $guard_y = $next_y;
  } while (is_inside($grid, $guard_y, $guard_x));

  return count($visited);
}

function main() {
  $grid = parse_grid();
  list($guard_y, $guard_x) = find_guard($grid);
  $n_visited = count_visited_positions($grid, $guard_y, $guard_x);
  echo "Part 1: $n_visited" . PHP_EOL;
}

$time0 = microtime(true);
main();
$time1 = microtime(true);
$millis = ($time1 - $time0) * 1000;
printf("Elapsed Time: %.3fms" . PHP_EOL, $millis);
