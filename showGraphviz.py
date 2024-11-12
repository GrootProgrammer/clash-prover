import os
import subprocess
from typing import List
import threading

with open("output.txt","r") as f:
    content = f.readlines()
    graph = False
    current_graph = []
    graphs = []
    for line in content:
        if line.startswith("starting graph:"):
            graph = True
            continue
        if "ending graph" in line:
            graph = False
            graphs.append(current_graph)
            current_graph = []
        if graph == True:
            current_graph.append(line)
            continue
    print(len(graphs))

if not os.path.isdir('graph'):
    os.mkdir('graph')

for file in os.listdir('graph'):
    os.remove('graph/' + file)

def recolor(graph : List[str], lines : List[str], color : str):
    #pass
    for i in range(0, len(graph)):
        if graph[i] in lines:
            graph[i] = f"{graph[i][:-2]}, color={color}]\n"

#for i in range(0, len(graphs)):
#    added = None
#    removed = None
#    if i < len(graphs)-1:
#        current_lines = set(graphs[i])
#        new_lines = set(graphs[i+1])
#        removed = current_lines - new_lines
#    if i > 0:
#        current_lines = set(graphs[i])
#        previous = set(graphs[i-1])
#        added = current_lines - previous
#    if removed is not None:
#        recolor(graphs[i], removed, 'red')
#    if added is not None:
#        recolor(graphs[i], added, 'green')
#    #if i > 0:
#    #    current_lines = set(graph[i])
#    #    previous_lines = set(graph[i-1])
#    #    diff = previous_lines - current_lines
#    #    removed = 

render_d3 = """<!DOCTYPE html>
<meta charset="utf-8">
<body>
<script src="https://d3js.org/d3.v7.min.js"></script>
<script src="https://unpkg.com/@hpcc-js/wasm@2.20.0/dist/graphviz.umd.js"></script>
<script src="https://unpkg.com/d3-graphviz@5.6.0/build/d3-graphviz.js"></script>
<div id="graph" style="text-align: center;"></div>
<script>

var dotIndex = 0;
var graphviz = d3.select("#graph").graphviz()
    .transition(function () {
        return d3.transition("main")
            .ease(d3.easeLinear)
            .delay(500)
            .duration(1500);
    })
    .logEvents(true)
    .on("initEnd", render);

function render() {
    var dotLines = dots[dotIndex];
    var dot = dotLines.join('');
    graphviz
        .renderDot(dot)
        .on("end", function () {
            dotIndex = (dotIndex + 1) % dots.length;
            render();
        });
}

var dots = [
"""

for g in graphs:
    render_d3 += "\n\t[\n\t\t\'digraph  {\',\n"
    for l in g:
        render_d3 += '\t\t\'\t' + l[1:-1] + '\',\n'
    render_d3 += "\t\t\'}\'\n\t],\n"
render_d3 += ']\n</script>'

with open('graph/d3.html', 'w') as animgraph:
    animgraph.write(render_d3)

graph_threads = []

for i in range(0, len(graphs)):
    with open(f"graph/{i}.dot", 'w') as graphdot:
        graphdot.write("digraph d {\n")
        graphdot.writelines(graphs[i])
        graphdot.write('}')

    def process(i):
        os.system(f'dot -Tsvg graph/{i}.dot > graph/{i}.svg')
        os.system(f'dot -Tpng graph/{i}.dot > graph/{i}.png')

    graph_threads.append(threading.Thread(target=process, args=(i,)))

_ = [i.start() for i in graph_threads]
_ = [i.join() for i in graph_threads]