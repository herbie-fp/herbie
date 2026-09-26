import subprocess

name = "hamming"
benchmark_dir = "bench/hamming"
results_dir = f"{name}_results"

subprocess.run(f"mkdir -p {results_dir}", shell=True, check=True)
subprocess.run(f"Racket -y src/main.rkt report --platform no-accelerators --seed 42 {benchmark_dir} {results_dir}/{name}out/ > {results_dir}/{name}_dump", shell=True, check=True)
subprocess.run(f"Python3 countsubexprs.py {results_dir}/{name}_dump > {results_dir}/{name}_top1k_subexprs", shell=True, check=True)
subprocess.run(f"Python3 toFpcore.py {results_dir}/{name}_top1k_subexprs > {results_dir}/{name}_subexr_fpcores", shell=True, check=True)
subprocess.run(f"Racket deduplicate.rkt {results_dir}/{name}_subexr_fpcores > {results_dir}/{name}_deduped", shell=True, check=True)
subprocess.run(f"Python3 toFpcore.py {results_dir}/{name}_deduped > {results_dir}/{name}_deduped_fpcores", shell=True, check=True)
subprocess.run(f"Racket get-errors.rkt {results_dir}/{name}_deduped_fpcores > {results_dir}/{name}_final", shell=True, check=True)