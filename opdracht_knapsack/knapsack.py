import random
import csv
import copy

"""
Knapsack Solver Implementation

This Python script provides a basic implementation of a knapsack problem 
solver along with various solving strategies. The knapsack problem 
is a classic optimization problem where given a set of items, each with a 
weight and a value, the goal is to determine the maximum value that can be 
accommodated in a knapsack of limited capacity.

Solvers:
- Solver_Random: Randomly shuffles items and evaluates knapsack solutions for 
a specified number of iterations.
- Solver_Optimal_Recursive: Solves the knapsack problem optimally
using a recursive approach.
- Solver_Optimal_Iterative_Deepcopy: Solves the knapsack problem 
optimally using an iterative approach with deep copies.
- Solver_Optimal_Iterative: Solves the knapsack problem optimally 
using an iterative approach without deep copying.
- Solver_Random_Improved: An improved random solver generating 
neighbor solutions by random removal and addition.

"""

class Item:
    def __init__(self, name, points, weight, volume, index):
        self.name = name
        self.points = points
        self.weight = weight
        self.volume = volume
        self.index = index
        
    def clone(self):
        return Item(self.name, self.points, self.weight, self.volume, 
                    self.index)


class Items:
    def __init__(self):
        self.items = []
        self.resources = Resources(0, 0, 0)
        self.global_index_counter = 1

    def add_item_knapsack(self, item):
        self.items.append(item)
    
    def add_item(self, item):
        item.index = self.global_index_counter
        self.global_index_counter += 1
        self.items.append(item)

    def clone(self):
        cloned_items = Items()
        cloned_items.global_index_counter = self.global_index_counter
        cloned_items.resources = self.resources.clone()

        for item in self.items:
            cloned_items.add_item(item.clone())
        return cloned_items

class Resources:
    def __init__(self, points, weight, volume):
        self.points = points
        self.weight = weight
        self.volume = volume

    def calc_items(self, item):
        if self.weight < item.weight or self.volume < item.volume:
            return False

        self.points += item.points
        self.weight -= item.weight
        self.volume -= item.volume
        return True

    def clone(self):
        return Resources(self.points, self.weight, self.volume)


class Knapsack:
    def __init__(self, points, weight, volume):
        self.resources = Resources(points, weight, volume)
        self.items = Items()

    def add_item(self, item):
        if self.resources.calc_items(item):
            self.items.add_item_knapsack(item)

    def get_items(self):
        return self.items

    def get_points(self):
        return self.resources.points
    
    def clone(self):
        cloned_knapsack = Knapsack(self.resources.points,
                                   self.resources.weight,
                                   self.resources.volume)
        cloned_knapsack.items = self.items.clone()
        return cloned_knapsack

    def save(self, filename):
        with open(filename, 'w') as file:
            file.write(f"Points:{self.resources.points}\n")

            for item in self.items.items:
                file.write(f"{item.name}\n")


def load_knapsack(filename):
    knapsack = Knapsack(0, 0, 0)
    items_list = Items()

    with open(filename, 'r') as file:
        csv_reader = csv.reader(file)

        for index, row in enumerate(csv_reader):
            name, points, weight, volume = row
            if name.lower() == "knapsack":
                knapsack.resources = Resources(int(points), int(weight),
                                               int(volume))
                print(knapsack)
            else:
                item = Item(name, int(points), int(weight), int(volume), index)
                items_list.add_item(item)

    return knapsack, items_list


class Solver_Random:
    """
    Randomly shuffles items and evaluates knapsack solutions for a specified
    number of iterations.
    """
    def __init__(self, iterations):
        self.iterations = iterations
        self.best_knapsack = None

    def solve(self, knapsack, items):
        self.best_knapsack = knapsack

        for _ in range(self.iterations):
            shuffled_items = items.items.copy()
            random.shuffle(shuffled_items)

            current_knapsack = Knapsack(knapsack.resources.points,
                                        knapsack.resources.weight,
                                        knapsack.resources.volume)

            for item in shuffled_items:
                current_knapsack.add_item(item)

            if current_knapsack.get_points() > self.best_knapsack.get_points():
                self.best_knapsack = current_knapsack

    def get_best_knapsack(self):
        return self.best_knapsack

       
class Solver_Optimal_Recursive:
    """
    Solves the knapsack problem optimally using a recursive approach.
    """
    def solve(self, knapsack, items):
        self.best_knapsack = knapsack
        self.recursive_search(knapsack, items, 0)

    def recursive_search(self, knapsack, items, depth):
        if depth == len(items.items):
            if knapsack.get_points() > self.best_knapsack.get_points():
                self.best_knapsack = knapsack.clone()
            return

        self.recursive_search(knapsack, items, depth + 1)

        item = items.items[depth]
        knapsack_with_item = knapsack.clone()
        knapsack_with_item.add_item(item)
        self.recursive_search(knapsack_with_item, items, depth + 1)

    def get_best_knapsack(self):
        return self.best_knapsack


class Solver_Optimal_Iterative_Deepcopy:
    """
    Solves the knapsack problem optimally using an iterative approach with
    deep copies.
    """
    def solve(self, knapsack, items):
        self.best_knapsack = knapsack
        stack = [(copy.deepcopy(knapsack), 0)]

        while stack:
            current_knapsack, depth = stack.pop()

            if depth == len(items.items):
                if current_knapsack.get_points() > self.best_knapsack.get_points():
                    self.best_knapsack = copy.deepcopy(current_knapsack)
                continue

            stack.append((copy.deepcopy(current_knapsack), depth + 1))

            item = items.items[depth]
            knapsack_with_item = copy.deepcopy(current_knapsack)
            knapsack_with_item.add_item(item)
            stack.append((knapsack_with_item, depth + 1))

    def get_best_knapsack(self):
        return self.best_knapsack


class Solver_Optimal_Iterative:
    """
    Solves the knapsack problem optimally using an iterative approach without
    deep copying.
    """
    def solve(self, knapsack, items):
        self.best_knapsack = knapsack
        stack = [(0, knapsack, 0)]

        while stack:
            depth, current_knapsack, current_item_index = stack.pop()

            if depth == len(items.items):
                if current_knapsack.get_points() > self.best_knapsack.get_points():
                    self.best_knapsack = current_knapsack
                continue

            stack.append((depth + 1, current_knapsack, current_item_index))

            item = items.items[current_item_index]
            if current_knapsack.resources.calc_items(item):
                knapsack_with_item = Knapsack(current_knapsack.resources.points,
                                             current_knapsack.resources.weight,
                                             current_knapsack.resources.volume)
                knapsack_with_item.items = current_knapsack.items.clone()
                knapsack_with_item.add_item(item)

                stack.append((depth + 1, knapsack_with_item, 
                                         current_item_index + 1))

        return self.best_knapsack
    
    def get_best_knapsack(self):
        return self.best_knapsack


class Solver_Random_Improved:
    """
    An improved random solver generating neighbor solutions by random removal 
    and addition.
    """
    def __init__(self, iterations):
        self.iterations = iterations
        self.best_knapsack = None

    def solve(self, knapsack, items):
        self.best_knapsack = knapsack

        for _ in range(self.iterations):
            neighbor_knapsack = self.generate_neighbor_solution(knapsack, items)
            
            if neighbor_knapsack.get_points() > self.best_knapsack.get_points():
                self.best_knapsack = neighbor_knapsack

    def generate_neighbor_solution(self, knapsack, items):
        neighbor_knapsack = Knapsack(knapsack.resources.points, 
                                     knapsack.resources.weight,
                                     knapsack.resources.volume)
        neighbor_knapsack.items.items = knapsack.items.items.copy()

        if neighbor_knapsack.items.items:
            item_to_remove = random.choice(neighbor_knapsack.items.items)
            neighbor_knapsack.remove_item(item_to_remove)

        item_to_add = random.choice(items.items)
        neighbor_knapsack.add_item(item_to_add)

        return neighbor_knapsack

    def get_best_knapsack(self):
        return self.best_knapsack


def main():
    solver_random = Solver_Random(1000)
    solver_optimal_recursive = Solver_Optimal_Recursive()
    solver_optimal_iterative_deepcopy = Solver_Optimal_Iterative_Deepcopy()
    solver_optimal_iterative = Solver_Optimal_Iterative()
    solver_random_improved = Solver_Random_Improved(5000)

    knapsack_file = "knapsack_small"
    print("=== solving:", knapsack_file)
    solve(solver_random, knapsack_file + ".csv", knapsack_file + "_solution_random.csv")
    solve(solver_optimal_recursive, knapsack_file + ".csv", knapsack_file + "_solution_optimal_recursive.csv")
    solve(solver_optimal_iterative_deepcopy, knapsack_file + ".csv",
          knapsack_file + "_solution_optimal_iterative_deepcopy.csv")
    solve(solver_optimal_iterative, knapsack_file + ".csv", knapsack_file + "_solution_optimal_iterative.csv")
    solve(solver_random_improved, knapsack_file + ".csv", knapsack_file + "_solution_random_improved.csv")

    knapsack_file = "knapsack_medium"
    print("=== solving:", knapsack_file)
    solve(solver_random, knapsack_file + ".csv", knapsack_file + "_solution_random.csv")
    solve(solver_optimal_recursive, knapsack_file + ".csv", knapsack_file + "_solution_optimal_recursive.csv")
    solve(solver_optimal_iterative_deepcopy, knapsack_file + ".csv",
          knapsack_file + "_solution_optimal_iterative_deepcopy.csv")
    solve(solver_optimal_iterative, knapsack_file + ".csv", knapsack_file + "_solution_optimal_iterative.csv")
    solve(solver_random_improved, knapsack_file + ".csv", knapsack_file + "_solution_random_improved.csv")

    knapsack_file = "knapsack_large"
    print("=== solving:", knapsack_file)
    solve(solver_random, knapsack_file + ".csv", knapsack_file + "_solution_random.csv")
    solve(solver_random_improved, knapsack_file + ".csv", knapsack_file + "_solution_random_improved.csv")


def solve(solver, knapsack_file, solution_file):
    """ Uses 'solver' to solve the knapsack problem in file
    'knapsack_file' and writes the best solution to 'solution_file'.
    """
    knapsack, items = load_knapsack(knapsack_file)
    solver.solve(knapsack, items)
    knapsack = solver.get_best_knapsack()
    print(f"saving solution with {knapsack.get_points()} points to '{solution_file}'")
    knapsack.save(solution_file)


if __name__ == "__main__": # keep this at the bottom of the file
    main()
