#!/usr/bin/env node

const { join } = require('path')
const fs = require('fs')

const INPUT_PATH = join(__dirname, '..', 'inputs', 'day08.txt')
const [X, Y] = [0, 1]

/** @typedef {[number, number]} Point */
/** @typedef {Map<string, Point[]>} FreqToPointsMap */
/** @typedef {Set<string>} StringSet */

const loadInput = (inputPath = INPUT_PATH) => {
  return fs.readFileSync(inputPath).toString().trim()
}

/**
 * @param {FreqToPointsMap} antennas
 * @param {string} freq
 * @param {Point} pos
 */
const pushValue = (antennas, freq, pos) => {
  points = antennas.get(freq)
  if (points === undefined) {
    antennas.set(freq, [pos])
  } else {
    points.push(pos)
  }
}

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
        if (freq !== '.') {
          pushValue(map, freq, [x, y])
        }
      }
    }

    return new this(map, width, height)
  }

  /**
   * @param {number}
   * @param {number}
   */
  _contains(x, y) {
    return 0 <= x && x < this._width && 0 <= y && y < this._height
  }

  countUniqueAntinodes(repeat = false) {
    /** @type {StringSet} */
    const antinodes = new Set()

    for (const freqPoints of this._antennas.values()) {
      const len = freqPoints.length

      for (let i = 0; i < len; i++) {
        const pi = freqPoints[i]
        for (let j = i + 1; j < len; j++) {
          const pj = freqPoints[j]
          const ans = this._getAntinodes(pi, pj, repeat)
          for (const an of ans) {
            antinodes.add(an.join(','))
          }
        }
      }
    }

    return antinodes.size
  }

  /**
   * @param {Point} p1
   * @param {Point} p2
   */
  _getAntinodes(p1, p2, repeat = false) {
    const dx = p2[X] - p1[X]
    const dy = p2[Y] - p1[Y]
    let a1x = p1[X] - dx
    let a1y = p1[Y] - dy
    let a2x = p2[X] + dx
    let a2y = p2[Y] + dy

    const ans = repeat ? [p1, p2] : []

    while (this._contains(a1x, a1y)) {
      ans.push([a1x, a1y])
      if (!repeat) {
        break
      }

      a1x -= dx
      a1y -= dy
    }

    while (this._contains(a2x, a2y)) {
      ans.push([a2x, a2y])
      if (!repeat) {
        break
      }

      a2x += dx
      a2y += dy
    }

    return ans
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
