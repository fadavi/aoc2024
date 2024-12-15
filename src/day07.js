#!/usr/bin/env bun

const { readFileSync } = require('fs')
const { resolve } = require('path')

const INPUT_FILE_PATH = resolve(__dirname, '..', 'inputs', 'day07.txt')

const parseInput = (inputFilePath = INPUT_FILE_PATH) => {
  return readFileSync(inputFilePath, 'utf-8')
    .trim()
    .split('\n')
    .map(ln => new Int32Array(ln.split(' ').map(s => parseInt(s))))
}

const isEquationPossible = (ops, acc = 0, index = 1) => {
  if (index === 1) {
    return isEquationPossible(ops, ops[1], index + 1)
  }
  if (index === ops.length) {
    return acc === ops[0]
  }

  const result = ops[0]
  const op = ops[index]
  ++index

  let nextAcc = acc + op
  if (nextAcc <= result && isEquationPossible(ops, nextAcc, index)) {
    return true
  }

  nextAcc = acc * op
  return nextAcc <= result && isEquationPossible(ops, nextAcc, index)
}

const main = () => {
  const sumResults = parseInput()
    .filter(ops => isEquationPossible(ops))
    .reduce((sum, ops) => sum + ops[0], 0)

  console.log(`Part 1: ${sumResults}`)
}

if (require.main === module) {
  const time0 = process.hrtime.bigint()
  main()
  const time1 = process.hrtime.bigint()
  const delta = Number(time1 - time0) / 1_000_000
  console.log(`Elapsed Time: ${delta}ms`)
}
