import json, html, sys

NUM_ITERATIONS = int(sys.argv[1])
REPORTS_PATH = sys.argv[2]
START_REPORT_PATH = REPORTS_PATH + "/start/results.json"
END_REPORT_PATH = REPORTS_PATH + "/end/results.json"
ACCELERATORS_PATH = REPORTS_PATH + "/accelerators.json"
OUTPUT_PATH = REPORTS_PATH + "/index.html"
START_FOLDER = "start"
END_FOLDER = "end"

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
    rows = []
    for acc in accelerators:
        name = html.escape(str(acc.get("name", "")))
        spec = html.escape(str(acc.get("spec", "")))
        count = counts.get(acc.get("name"), 0)
        if count == 0:
            continue
        rows.append(f'<tr><td>{name}</td><td><code>{spec}</code></td><td>{count}</td></tr>')
    return "".join(rows)

end_report = load_report(END_REPORT_PATH)

start_accuracy_str = get_accuracy_str(START_REPORT_PATH)
end_accuracy_str = get_accuracy_str(END_REPORT_PATH)
accelerators = load_accelerators(ACCELERATORS_PATH)

def read_text(path):
    with open(path, 'r') as f:
        return f.read()

counts = {
    acc_name: read_text(END_REPORT_PATH).count(acc_name)
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
    <p><a href="expr_dump.txt">Herbie Dump</a></p>
    <p><a href="full-candidates.txt">All Candidates</a></p>
    <p><a href="candidates.txt">Top 500 Candidates</a></p>
    <p><a href="counts.json">Candidate Counts</a></p>
    <p><a href="costs.json">Candidate Costs</a></p>
    <p><a href="info.txt">More info</a></p>
    <p><a href="accelerators.json">Accelerators</a></p>

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

with open(OUTPUT_PATH, 'w') as f:
    f.write(html_content)
