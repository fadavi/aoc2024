#!/usr/bin/env php
<?php

define('INPUT_FILE_PATH', __DIR__ . '/../inputs/day06.txt');

define('FLOOR', ord('.'));
define('OBSTACLE', ord('#'));
define('UP', 0b0001);
define('DOWN', 0b0010);
define('LEFT', 0b0100);
define('RIGHT', 0b1000);

define('TURN_90DEG', [
  UP => RIGHT,
  RIGHT => DOWN,
  DOWN => LEFT,
  LEFT => UP,
]);

function parse_grid(string $path = INPUT_FILE_PATH): array
{
  $content = trim(file_get_contents($path));
  return array_map(
    fn($row) => array_map('ord', str_split($row)),
    explode(PHP_EOL, $content),
  );
}

function find_guard(array &$grid): array
{
  $caret_code = ord('^');
  foreach ($grid as $y => $row) {
    $x = array_search($caret_code, $row);
    if ($x !== false) {
      return [$y, $x];
    }
  }
}

function step_forward(int $y, int $x, int $dir): array
{
  return match ($dir) {
    UP => [$y - 1, $x],
    DOWN => [$y + 1, $x],
    LEFT => [$y, $x - 1],
    RIGHT => [$y, $x + 1],
  };
}

function get_next_pos(
  array &$grid,
  int $guard_y,
  int $guard_x,
  int $guard_dir,
): array {
  [$next_y, $next_x] = step_forward($guard_y, $guard_x, $guard_dir);

  if (!is_obstacle($grid, $next_y, $next_x)) {
    return [$next_y, $next_x, $guard_dir];
  }

  return get_next_pos($grid, $guard_y, $guard_x, TURN_90DEG[$guard_dir]);
}

function is_inside(array &$grid, int $y, int $x): bool
{
  return 0 <= $y && $y < count($grid) && 0 <= $x && $x < count($grid[0]);
}

function is_obstacle(array &$grid, int $y, int $x): bool
{
  return ($grid[$y][$x] ?? FLOOR) === OBSTACLE;
}

function hash_point(int $y, int $x)
{
  return ($y << 10) | $x;
}

function is_in_loop(
  array &$grid,
  array &$visited,
  int $guard_y,
  int $guard_x,
  int $guard_dir,
): bool {
  $visited_here = [];

  do {
    [$guard_y, $guard_x, $guard_dir] = get_next_pos(
      $grid,
      $guard_y,
      $guard_x,
      $guard_dir,
    );

    $next_encoded = hash_point($guard_y, $guard_x);
    $visited_dir = $visited[$next_encoded] ?? $visited_here[$next_encoded] ?? 0;
    if ($visited_dir === $guard_dir) {
      return true;
    }

    $visited_here[$next_encoded] = $guard_dir;
  } while (is_inside($grid, $guard_y, $guard_x));

  return false;
}

function get_visited_positions(array &$grid): array {
  [$guard_y, $guard_x] = find_guard($grid);
  $guard_dir = UP;

  $visited = [
    hash_point($guard_y, $guard_x) => $guard_dir,
  ];
  $loops = [];

  for(;;) {
    [$next_y, $next_x, $next_dir] = get_next_pos(
      $grid,
      $guard_y,
      $guard_x,
      $guard_dir,
    );
    if (!is_inside($grid, $next_y, $next_x)) {
      break;
    }

    $next_encoded = hash_point($next_y, $next_x);
    $already_visited = isset($visited[$next_encoded]);

    if (!$already_visited) {
      $grid[$next_y][$next_x] = OBSTACLE;
      if (is_in_loop($grid, $visited, $guard_y, $guard_x, $guard_dir)) {
        $loops[$next_encoded] = true;
      }
      $grid[$next_y][$next_x] = FLOOR;
    }

    $visited[$next_encoded] = $next_dir;
    $guard_y = $next_y;
    $guard_x = $next_x;
    $guard_dir = $next_dir;
  }

  return [count($visited), count($loops)];
}

function main() {
  $grid = parse_grid();
  [$n_visited, $n_loops] = get_visited_positions($grid);

  echo "Part 1: $n_visited" . PHP_EOL;
  echo "Part 2: $n_loops" . PHP_EOL;
}

$time0 = microtime(true);
main();
$time1 = microtime(true);
$millis = ($time1 - $time0) * 1000;
printf("Elapsed Time: %.3fms" . PHP_EOL, $millis);
