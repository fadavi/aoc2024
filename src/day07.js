#!/usr/bin/env bun

const { readFileSync } = require('fs')
const { resolve } = require('path')

const INPUT_FILE_PATH = resolve(__dirname, '..', 'inputs', 'day07.txt')

const parseInput = (inputFilePath = INPUT_FILE_PATH) => {
  return readFileSync(inputFilePath, 'utf-8')
    .trim()
    .split('\n')
    .map(ln => ln.split(' ').map(s => parseInt(s)))
}

/**
 * @param {number} a
 * @param {number} b
 */
const concat = (a, b) => {
  if (b < 1e1) {
    return a * 1e1 + b
  } else if (b < 1e2) {
    return a * 1e2 + b
  } else if (b < 1e3) {
    return a * 1e3 + b
  } else if (b < 1e4) {
    return a * 1e4 + b
  } else if (b < 1e5) {
    return a * 1e5 + b
  } else if (b < 1e6) {
    return a * 1e6 + b
  } else if (b < 1e7) {
    return a * 1e7 + b
  }

  const digits = Math.floor(Math.log10(b)) + 1
  return a * 10 ** digits + b
}

/**
 * @param {number[]} ops
 * @param {number} acc
 * @param {number} index
 * @param {boolean} testConcat
 */
const isEquationPossible = (ops, acc, index, testConcat) => {
  const result = ops[0]
  const op = ops[index]

  if (index === ops.length) {
    return acc === result
  }

  ++index

  let nextAcc = acc + op
  if (nextAcc <= result && isEquationPossible(ops, nextAcc, index, testConcat)) {
    return true
  }

  nextAcc = acc * op
  if (nextAcc <= result && isEquationPossible(ops, nextAcc, index, testConcat)) {
    return true
  }

  if (!testConcat) {
    return false
  }

  nextAcc = concat(acc, op)
  return nextAcc <= result && isEquationPossible(ops, nextAcc, index, testConcat)
}

/**
 * @param {number[][]} equations
 * @param {boolean} testConcat
 */
const getSumOfValidResults = (equations, testConcat) => {
  return equations
    .filter(ops => isEquationPossible(ops, ops[1], 2, testConcat))
    .reduce((sum, ops) => sum + ops[0], 0)
}

const main = () => {
  const equations = parseInput()
  const parts = process.argv[2] || 'part1,part2'

  if (parts.includes('part1')) {
    const sumResultsWithoutConcat = getSumOfValidResults(equations, false)
    console.log(`Part 1: ${sumResultsWithoutConcat}`)
  }

  if (parts.includes('part2')) {
    const sumResultsWithConcat = getSumOfValidResults(equations, true)
    console.log(`Part 2: ${sumResultsWithConcat}`)
  }
}

if (require.main === module) {
  const time0 = performance.now()
  main()
  const time1 = performance.now()
  const delta = time1 - time0
  console.log(`Elapsed Time: ${delta}ms`)
}
