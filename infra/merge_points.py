from pathlib import Path
import shutil


def unique_path_for(out_dir, base_name):
    base = f"{base_name}.json"
    candidate = out_dir / base
    if not candidate.exists():
        return candidate
    idx = 1
    while True:
        candidate = out_dir / f"{base_name}_{idx}.json"
        if not candidate.exists():
            return candidate
        idx += 1


def main():
    report_dir = Path("report")
    out_dir = Path("sampled-points")
    out_dir.mkdir(parents=True, exist_ok=True)
    for src in report_dir.rglob("all-points.json"):
        dest = unique_path_for(out_dir, src.parent.name or "root")
        shutil.copy2(src, dest)
        print(dest.name)


if __name__ == "__main__":
    main()
