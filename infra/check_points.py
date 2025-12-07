import os
import json

base = "sampled-points"
main = "sampled-points-main"

for root, _, files in os.walk(base):
    n_same, n_diff, n_error = 0, 0, 0

    for name in sorted(files):
        if not name.endswith(".json"):
            continue

        path = os.path.join(root, name)
        rel = os.path.relpath(path, base)
        other = os.path.join(main, rel)

        if not os.path.exists(other):
            print(f"[SKIP] {rel} (not in {main})")
            continue

        try:
            a, b = json.load(open(path)), json.load(open(other))
        except json.JSONDecodeError:
            print(f"[ERROR] {rel} (invalid json)")
            n_error += 1
            continue

        a_sorted = sorted(a, key=lambda x: tuple(x[0]))
        b_sorted = sorted(b, key=lambda x: tuple(x[0]))

        if a_sorted == b_sorted:
            print(f"[SAME] {rel}")
            n_same += 1
        else:
            print(f"[DIFF] {rel}")
            n_diff += 1
            for point_a, point_b in zip(a_sorted, b_sorted):
                args_a, res_a = point_a
                args_b, res_b = point_b
                if args_a != args_b:
                    print(f"    Different arguments found: {args_a} vs {args_b}")
                    break
                if res_a != res_b:
                    print(f"    Args: {args_a}")
                    print(f"    Base result: {res_a}")
                    print(f"    Main result: {res_b}")
                    break

    print(f"Summary: {n_same} same, {n_diff} different, {n_error} errors in {root}")
