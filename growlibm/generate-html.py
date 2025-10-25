import json
import html

START_REPORT_PATH = 'reports/start/results.json'
END_REPORT_PATH = 'reports/end/results.json'
START_FOLDER = 'start'
END_FOLDER = 'end'
OUTPUT_FILE = 'reports/index.html'
NUM_ITERATIONS = 10 
ACCELERATORS_PATH = 'reports/accelerators.json'

def calculate_end_accuracy(data):
    total_end = 0
    max_accuracy = 0
    for test in data.get("tests", []):
        total_end += test.get("end", 0)
        max_accuracy += test.get("bits", 0)
    if max_accuracy == 0:
        return 0.0
    return 100 - (100 * (total_end / max_accuracy))

def load_report(path):
    with open(path, 'r') as f:
        return json.load(f)

def get_accuracy_str(path):
    report = load_report(path)
    percent = calculate_end_accuracy(report)
    return f"{percent:.1f}%"

def load_accelerators(path):
    with open(path, 'r') as f:
        data = json.load(f)
    if isinstance(data, dict):
        return [data]
    if isinstance(data, (list, tuple, set)):
        return list(data)
    return []


def format_accelerator_rows(accelerators, counts):
    if not accelerators:
        return '<tr><td colspan="3">No accelerators recorded.</td></tr>'
    rows = []
    for acc in accelerators:
        name = html.escape(str(acc.get("name", "")))
        spec = html.escape(str(acc.get("spec", "")))
        count = counts.get(acc.get("name"), 0)
        rows.append(f'<tr><td>{name}</td><td><code>{spec}</code></td><td>{count}</td></tr>')
    return "".join(rows)

end_report = load_report(END_REPORT_PATH)

start_accuracy_str = get_accuracy_str(START_REPORT_PATH)
end_accuracy_str = get_accuracy_str(END_REPORT_PATH)
accelerators = load_accelerators(ACCELERATORS_PATH)

def read_text(path):
    with open(path, 'r') as f:
        return f.read()

report_paths = [END_REPORT_PATH] + [f"reports/iter{i}/results.json" for i in range(NUM_ITERATIONS)]
report_texts = [read_text(p) for p in report_paths]
counts = {
    acc_name: sum(text.count(acc_name) for text in report_texts)
    for acc_name in [str(acc.get("name", "")) for acc in accelerators]
    if acc_name
}

accelerator_rows = format_accelerator_rows(accelerators, counts)

table_rows = "".join([
    f'<tr><td><a href="iter{i}/">iter{i}</a></td><td>{get_accuracy_str(f"reports/iter{i}/results.json")}</td></tr>'
    for i in range(NUM_ITERATIONS)
])

html_content = f"""
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>Growlibm Report</title>
</head>
<body>
    <h1>GrowLibm Report</h1>
    <p><a href="grow_platform.txt">Grow Platform</a></p>
    <p><a href="candidates.txt">Candidates</a></p>
    
    <h2>Overall Summary</h2>
    <p><strong>Start:</strong> {start_accuracy_str} - <a href="{START_FOLDER}/">Folder</a></p>
    <p><strong>End:</strong> {end_accuracy_str} - <a href="{END_FOLDER}/">Folder</a></p>
    
    <h2>Accelerators</h2>
    <table border="1" style="border-collapse:collapse; width:100%; max-width:900px;">
        <tr>
            <th>Name</th>
            <th>Spec</th>
            <th>Uses</th>
        </tr>
        {accelerator_rows}
    </table>
    
    <h2>Grow Platform Iterations</h2>
    <table border="1" style="border-collapse:collapse; width:300px;">
        <tr>
            <th>Iteration</th>
            <th>Accuracy</th>
        </tr>
        {table_rows}
    </table>
</body>
</html>
"""

with open(OUTPUT_FILE, 'w') as f:
    f.write(html_content)
