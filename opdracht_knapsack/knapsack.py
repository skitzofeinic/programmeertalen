import random
import csv
import os
import copy

class Item:
    def __init__(self, name, points, weight, volume, index):
        self.name = name
        self.points = points
        self.weight = weight
        self.volume = volume
        self.index = index

class Items:
    def __init__(self):
        self.items = []
        self.resources = Resources(0, 0, 0)
        self.index_counter = 0

    def add_item(self, item):
        item.index = self.index_counter
        self.index_counter += 1
        self.items.append(item)

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

class Knapsack:
    def __init__(self, points, weight, volume):
        self.resources = Resources(points, weight, volume)
        self.items = Items()

    def add_item(self, item):
        if self.resources.calc_items(item):
            self.items.add_item(item)

    def get_items(self):
        return self.items

    def get_points(self):
        return self.resources.points

    def save(self, filename):
        with open(filename, 'w') as file:
            file.write(f"Points:{self.resources.points}, Remaining weight:{self.resources.weight}, Remaining volume:{self.resources.volume}\n")

            for item in self.items.items:
                file.write(f"{item.name}, {item.points}, {item.weight}, {item.volume}\n")


def load_knapsack(filename):
    knapsack = Knapsack(0, 0, 0)
    items_list = Items()

    with open(filename, 'r') as file:
        csv_reader = csv.reader(file)
        header = next(csv_reader)

        for index, row in enumerate(csv_reader):
            name, points, weight, volume = row
            if name.lower() == "knapsack":
                knapsack.resources = Resources(int(points), int(weight), int(volume))
                print(knapsack)
            else:
                item = Item(name, int(points), int(weight), int(volume), index)
                items_list.add_item(item)

    return knapsack, items_list

class Solver_Random:
    def __init__(self, iterations):
        self.iterations = iterations
        self.best_knapsack = None

    def solve(self, knapsack, items):
        self.best_knapsack = knapsack

        for _ in range(self.iterations):
            shuffled_items = items.items.copy()
            random.shuffle(shuffled_items)

            current_knapsack = Knapsack(knapsack.resources.points, knapsack.resources.weight, knapsack.resources.volume)

            for item in shuffled_items:
                current_knapsack.add_item(item)

            if current_knapsack.get_points() > self.best_knapsack.get_points():
                self.best_knapsack = current_knapsack

    def get_best_knapsack(self):
        return self.best_knapsack
            
class Solver_Optimal_Recursive:
    pass

class Solver_Optimal_Iterative_Deepcopy:
    pass

class Solver_Optimal_Iterative:
    pass

class Solver_Random_Improved:
    pass
        
def main():
    solver_random = Solver_Random(1000)
    solver_optimal_recursive = Solver_Optimal_Recursive()
    # solver_optimal_iterative_deepcopy = Solver_Optimal_Iterative_Deepcopy()
    # solver_optimal_iterative = Solver_Optimal_Iterative()
    # solver_random_improved = Solver_Random_Improved(5000)

    script_dir = os.path.dirname(os.path.abspath(__file__))
    knapsack_file = os.path.join(script_dir, 'knapsack_small.csv')
    solve(solver_random, knapsack_file, knapsack_file.replace('.csv', '_solution_random.csv'))
    # knapsack_file = "knapsack_small"
    print("=== solving:", knapsack_file)
    # solve(solver_random, knapsack_file, knapsack_file + "_solution_random.csv")
    # solve(solver_optimal_recursive, knapsack_file + ".csv", knapsack_file + "_solution_optimal_recursive.csv")
    # solve(solver_optimal_iterative_deepcopy, knapsack_file + ".csv",
    #       knapsack_file + "_solution_optimal_iterative_deepcopy.csv")
    # solve(solver_optimal_iterative, knapsack_file + ".csv", knapsack_file + "_solution_optimal_iterative.csv")
    # solve(solver_random_improved, knapsack_file + ".csv", knapsack_file + "_solution_random_improved.csv")
    

    knapsack_file = os.path.join(script_dir, 'knapsack_medium.csv')
    solve(solver_random, knapsack_file, knapsack_file.replace('.csv', '_solution_random.csv'))
    print("=== solving:", knapsack_file)
    # solve(solver_random, knapsack_file + ".csv", knapsack_file + "_solution_random.csv")
    # solve(solver_optimal_recursive, knapsack_file + ".csv", knapsack_file + "_solution_optimal_recursive.csv")
    # solve(solver_optimal_iterative_deepcopy, knapsack_file + ".csv",
    #       knapsack_file + "_solution_optimal_iterative_deepcopy.csv")
    # solve(solver_optimal_iterative, knapsack_file + ".csv", knapsack_file + "_solution_optimal_iterative.csv")
    # solve(solver_random_improved, knapsack_file + ".csv", knapsack_file + "_solution_random_improved.csv")

    knapsack_file = os.path.join(script_dir, 'knapsack_large.csv')
    solve(solver_random, knapsack_file, knapsack_file.replace('.csv', '_solution_random.csv'))
    print("=== solving:", knapsack_file)
    # solve(solver_random, knapsack_file + ".csv", knapsack_file + "_solution_random.csv")
    # solve(solver_random_improved, knapsack_file + ".csv", knapsack_file + "_solution_random_improved.csv")


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
