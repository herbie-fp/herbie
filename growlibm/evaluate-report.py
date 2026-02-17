import json, os, sys

class report:
    def __init__(self, accelerators, path) -> None:
        self.accelerators = accelerators
        self.path = path

reports_path = sys.argv[1]
platforms = ["vanilla", "herbie20", "growlibm"] 
options = ["base"]
accelerators = ["sinprod", "cosprod", "sinquot", "cosquot", "log1pmd", "invgud", "hypot", "verdcos"]
# accelerators = ["powcos", "powcos2", "powcos4", "powcos6", "ncos1p"]
output_path = reports_path + "/index.html"
reports = []

for p in platforms:
    for o in options:
        r = report(p == "growlibm" or p == "herbie20plus", p + "_" + o)
        reports.append(r)
reports.append(report(True, "growlibmbest_base"))

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
    if not os.path.exists(path):
        return "n/a"
    report = load_report(path)
    percent = calculate_end_accuracy(report)
    return f"{percent:.1f}%"

def format_accelerator_rows(accelerators, counts):
    rows = []
    for acc in accelerators:
        count = counts.get(acc + '.', 0)
        rows.append(f'<tr><td>{acc}</td><td>{count}</td></tr>')
    return "".join(rows)

def read_text(path):
    with open(path, 'r') as f:
        return f.read()

def get_counts(path):
    if not os.path.exists(path):
        return {acc_name: 0 for acc_name in accelerators}
    return {acc_name: read_text(path).count(acc_name) for acc_name in accelerators}

accuracy_rows = []
accelerator_rows = []
for r in reports:
    results_path = f"{reports_path}/{r.path}/results.json"
    accuracy_rows.append(f"<li><a href={r.path}>{r.path}: </a> {get_accuracy_str(results_path)}</li> ")
    if r.accelerators:
        accelerator_rows.append(f'<tr><th colspan="2">{r.path}</th></tr>')
        accelerator_rows.append(
            format_accelerator_rows(
                accelerators,
                get_counts(f"{reports_path}/{r.path}/results.json"),
            )
        )

accuracy_html = "".join(accuracy_rows)
accelerator_html = "".join(accelerator_rows)

html_content = f"""
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>Comparing Platforms</title>
</head>
<body>    
    <h2>Evaluation</h2>
    <ul>
        {accuracy_html}
    </ul>
    
    <h2>Accelerators</h2>
    <table border="1" style="border-collapse:collapse; width:100%; max-width:900px;">
        <tr>
            <th>Name</th>
            <th>Uses</th>
        </tr>
        {accelerator_html}
    </table>
    

</body>
</html>
"""

with open(output_path, 'w') as f:
    f.write(html_content)
