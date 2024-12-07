#!/usr/bin/env lua

local SEP = '/'
local INPUT_PATH = 'inputs' .. SEP .. 'day05.txt' -- Relative to the *root* dir.

---@param nums number[]
---@return number
local function sum_list(nums)
  local sum = 0

  for _, n in ipairs(nums) do
    sum = sum + n
  end

  return sum
end

---@param str string
---@param delim string
---@return table
local function split_string(str, delim)
  local result = {}
  for match in (str .. delim):gmatch('(.-)' .. delim) do
    table.insert(result, match)
  end
  return result
end

---@param str string
---@param delim string
---@return integer[]
local function parse_integers(str, delim)
  ---@type integer[]
  local result = {}

  for _, val in ipairs(split_string(str, delim)) do
    table.insert(result, tonumber(val))
  end

  return result
end

---NOTE: the list will be mutated.
---@param list any[]
---@param idx1 integer
---@param idx2 integer
local function swap_in_place(list, idx1, idx2)
  local tmp = list[idx1]
  list[idx1] = list[idx2]
  list[idx2] = tmp
end

---@param path string
---@return string[]
local function read_input(path)
  local file = io.open(path, 'r')
  if not file then
    error('File ' .. path .. ' not found.')
  end

  local lines = {}
  for line in file:lines() do
    table.insert(lines, line)
  end

  file:close()
  return lines
end

---@param input_lines string[]
local function parse_input(input_lines)
  ---@type table
  local rule_pairs = {}

  ---@type table
  local updates = {}

  ---@type string
  local line = table.remove(input_lines, 1)

  while line ~= '' do
    table.insert(rule_pairs, parse_integers(line, '|'))
    line = table.remove(input_lines, 1)
  end

  line = table.remove(input_lines, 1)

  while line ~= nil do
    table.insert(updates, parse_integers(line, ','))
    line = table.remove(input_lines, 1)
  end

  return rule_pairs, updates
end

---@param rule_pairs integer[]
local function compile_rules(rule_pairs)
  local rules = {}

  for _, pair in ipairs(rule_pairs) do
    local a = pair[1]
    local b = pair[2]
    local rule = rules[a] or {}
    rule[b] = true -- means: `a` MUST be printed BEFORE `b`
    rules[a] = rule
  end

  return rules
end

---@param rules table
---@param left integer
---@param right integer
local function is_valid(rules, left, right)
  local rule = rules[right]
  if not rule then
    return true
  end

  return not rule[left]
end

---@param rules table
---@param update integer[]
local function is_in_correct_order(rules, update)
  for l_idx = 1, #update do
    local left = update[l_idx]
    for r_idx = l_idx + 1, #update do
      local right = update[r_idx]

      if not is_valid(rules, left, right) then
        return false
      end
    end
  end

  return true
end

---@param rules table
---@param update integer[]
local function fix_update_in_place(rules, update)
  for l_idx = 1, #update do
    local left = update[l_idx]
    for r_idx = l_idx + 1, #update do
      local right = update[r_idx]

      if not is_valid(rules, left, right) then
        swap_in_place(update, l_idx, r_idx)
        fix_update_in_place(rules, update)
        return true
      end
    end
  end

  return false
end

---@param update integer[]
local function get_middle_page_number(update)
  local mid_idx = math.ceil(#update / 2)
  return update[mid_idx]
end

---@param rules table
---@param updates integer[][]
local function get_middle_page_numbers_of_valid_updates(rules, updates)
  local page_numbers = {}

  for _, update in ipairs(updates) do
    if is_in_correct_order(rules, update) then
      table.insert(page_numbers, get_middle_page_number(update))
    end
  end

  return page_numbers
end

local function get_middle_page_numbers_after_fixing(rules, updates)
  local page_numbers = {}

  for _, update in ipairs(updates) do
    local fix_needed = fix_update_in_place(rules, update)
    if fix_needed then
      table.insert(page_numbers, get_middle_page_number(update))
    end
  end

  return page_numbers
end

local function main()
  local input = read_input(INPUT_PATH)
  local rule_pairs, updates = parse_input(input)
  local rules = compile_rules(rule_pairs)

  local mid_pages_of_correct_updates = get_middle_page_numbers_of_valid_updates(
    rules,
    updates
  )
  local sum_for_correct_updates = sum_list(mid_pages_of_correct_updates)
  print('Part1: ' .. sum_for_correct_updates)

  local mid_pages_of_fixed_updates = get_middle_page_numbers_after_fixing(
    rules,
    updates
  )
  local sum_for_fixed_updates = sum_list(mid_pages_of_fixed_updates)
  print('Part2: ' .. sum_for_fixed_updates)
end

main()
