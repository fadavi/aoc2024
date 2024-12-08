#!/usr/bin/env node

const { readFileSync } = require('fs')
const { join } = require('path')

const INPUT_PATH = join(__dirname, '..', 'inputs', 'day08.txt')
const [X, Y] = [0, 1]

const EMPTY_SPOT = '.'

/** @typedef {[number, number]} Point */
/** @typedef {Map<string, Point[]>} FreqToPointsMap */
/** @typedef {Set<number>} HashSet */

const loadInput = (inputPath = INPUT_PATH) => {
  return readFileSync(inputPath, 'utf-8').trim()
}

/**
 * @template K, V
 * @param {Map<K, V[]>} map
 * @param {K} key
 * @param {V} val
 */
const pushValueToMap = (map, key, val) => {
  points = map.get(key)
  if (points) {
    points.push(val)
  } else {
    map.set(key, [val])
  }
}

/** @param {Point} p */
const hashPoint = (p) => (p[X] << 16) ^ p[Y]

class AntennasMap {
  /**
   * @param {FreqToPointsMap} map
   * @param {number} width
   * @param {number} height
   */
  constructor(map, width, height) {
    /** @type {FreqToPointsMap} */
    this._antennas = map

    /** @type {number} */
    this._width = width

    /** @type {number} */
    this._height = height
  }

  /**
   * @param {number}
   * @param {number}
   */
  _contains(x, y) {
    return 0 <= x && x < this._width && 0 <= y && y < this._height
  }

  /**
   * @param {Point} p1
   * @param {Point} p2
   */
  _getAntinodes(p1, p2, repeat = false) {
    const dx = p2[X] - p1[X]
    const dy = p2[Y] - p1[Y]
    let ax = p1[X] - dx
    let ay = p1[Y] - dy

    const antinodes = repeat ? [p1, p2] : []

    while (this._contains(ax, ay)) {
      antinodes.push([ax, ay])
      if (!repeat) {
        break
      }

      ax -= dx
      ay -= dy
    }

    ax = p2[X] + dx
    ay = p2[Y] + dy

    while (this._contains(ax, ay)) {
      antinodes.push([ax, ay])
      if (!repeat) {
        break
      }

      ax += dx
      ay += dy
    }

    return antinodes
  }

  countUniqueAntinodes(repeat = false) {
    /** @type {HashSet} */
    const antinodes = new Set()

    for (const freqPoints of this._antennas.values()) {
      const len = freqPoints.length

      for (let j, i = 0; i < len; ++i) {
        const pi = freqPoints[i]
        for (j = i + 1; j < len; ++j) {
          const pj = freqPoints[j]
          const ans = this._getAntinodes(pi, pj, repeat)
          for (const an of ans) {
            antinodes.add(hashPoint(an))
          }
        }
      }
    }

    return antinodes.size
  }

  /** @param {string} gridStr */
  static parse(gridStr) {
    /** @type {FreqToPointsMap} */
    const map = new Map()
    const rows = gridStr.split('\n').map(line => line.split(''))

    const height = rows.length
    let width = 0

    for (const [y, row] of rows.entries()) {
      width = Math.max(width, row.length)

      for (const [x, freq] of row.entries()) {
        if (freq !== EMPTY_SPOT) {
          pushValueToMap(map, freq, [x, y])
        }
      }
    }

    return new this(map, width, height)
  }
}

const main = () => {
  const input = loadInput()
  const map = AntennasMap.parse(input)

  const parts = process.argv[2] || 'part1,part2'

  if (parts.includes('part1')) {
    const nAntinodes = map.countUniqueAntinodes()
    console.log('Part 1:', nAntinodes)
  }

  if (parts.includes('part2')) {
    const nAntinodesRepeat = map.countUniqueAntinodes(true)
    console.log('Part 2:', nAntinodesRepeat)
  }
}

if (require.main === module) {
  main()
}
