const fs = require('fs')
const { join } = require('path')

const INPUT_PATH = join(__dirname, '..', 'inputs', 'day08.txt')
const [X, Y] = [0, 1]

/** @typedef {[number, number]} Point */
/** @typedef {Map<string, Point[]>} AntennasMap */
/** @typedef {Set<string>} AntinodeSet */

const loadInput = (inputPath = INPUT_PATH) => {
  return fs.readFileSync(inputPath).toString().trim()
}

/**
 * @param {AntennasMap} antennas
 * @param {string} freq
 * @param {Point} pos
 */
const appendAntenna = (antennas, freq, pos) => {
  points = antennas.get(freq)
  if (points === undefined) {
    antennas.set(freq, [pos])
  } else {
    points.push(pos)
  }
}

/**
 * @param {Point} p1
 * @param {Point} p2
 */
const getAntinodes = (p1, p2) => {
  const dx = p2[X] - p1[X]
  const dy = p2[Y] - p1[Y]
  return [
    [p1[X] - dx, p1[Y] - dy],
    [p2[X] + dx, p2[Y] + dy],
  ]
}

class AntennasMap {
  /**
   * @param {AntennasMap} map
   * @param {number} width
   * @param {number} height
   */
  constructor(map, width, height) {
    /** @type {AntennasMap} */
    this._antennas = map

    /** @type {number} */
    this._width = width

    /** @type {number} */
    this._height = height
  }

  /** @param {string} gridStr */
  static parse(gridStr) {
    const map = new Map()
    const rows = gridStr.split('\n').map(line => line.split(''))

    const height = rows.length
    let width = 0

    for (const [y, row] of rows.entries()) {
      width = Math.max(width, row.length)

      for (const [x, freq] of row.entries()) {
        if (freq !== '.') {
          appendAntenna(map, freq, [x, y])
        }
      }
    }

    return new AntennasMap(map, width, height)
  }

  /** @param {Point} _ */
  contains([x, y]) {
    return 0 <= x && x < this._width && 0 <= y && y < this._height
  }

  countUniqueAntinodes() {
    const antinodes = new Set()

    for (const freqPoints of this._antennas.values()) {
      const len = freqPoints.length

      for (let i = 0; i < len; i++) {
        const pi = freqPoints[i]
        for (let j = i + 1; j < len; j++) {
          const pj = freqPoints[j]
          const [an1, an2] = getAntinodes(pi, pj)

          if (this.contains(an1)) {
            antinodes.add(an1.join(','))
          }

          if (this.contains(an2)) {
            antinodes.add(an2.join(','))
          }
        }
      }
    }

    return antinodes.size
  }
}

const main = () => {
  const input = loadInput()
  const map = AntennasMap.parse(input)

  const nAntinodes = map.countUniqueAntinodes()
  console.log('Part 1:', nAntinodes)
}

if (require.main === module) {
  main()
}
