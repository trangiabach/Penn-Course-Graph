import './App.css';
import Graph from 'vis-react';
import initialGraph from "./data.json";
import React, { useEffect, useState } from 'react';
import { Box, Typography, Chip, Stack } from '@mui/material';
import OutlinedInput from '@mui/material/OutlinedInput';
import InputLabel from '@mui/material/InputLabel';
import MenuItem from '@mui/material/MenuItem';
import FormControl from '@mui/material/FormControl';
import ListItemText from '@mui/material/ListItemText';
import Select, { SelectChangeEvent } from '@mui/material/Select';
import Checkbox from '@mui/material/Checkbox';
import { createTheme, ThemeProvider } from '@mui/material/styles';

var highlightActive = false;
let options = {
  layout: {
    randomSeed: 2,
    improvedLayout: false
  },
  nodes: {
    fixed: {
      x: false,
      y: false
    },
    shape: "dot",
    size: 10,
    borderWidth: 1.5,
    borderWidthSelected: 2,
    font: {
      size: 15,
      align: "center",
      bold: {
        color: "#bbbdc0",
        size: 15,
        vadjust: 0,
        mod: "bold"
      }
    },
    shapeProperties: {
      interpolation: false    // 'true' for intensive zooming
    }
  },
  edges: {
    width: 0.01,
    color: {
      color: "#D3D3D3",
      highlight: "#797979",
      hover: "#797979",
      opacity: 1.0
    },
    smooth: {
      type: "continuous",
      roundness: 0
    }
  },
  // physics: {
  //   enabled: true,
  // },
  physics: {
    forceAtlas2Based: {
        gravitationalConstant: -26,
        centralGravity: 0.005,
        springLength: 230,
        springConstant: 0.18,
        avoidOverlap: 1.5
    },
    maxVelocity: 146,
    solver: 'forceAtlas2Based',
    timestep: 0.35,
    stabilization: {
        enabled: true,
        iterations: 1000,
        updateInterval: 25
    }
  },
  // physics: {
  //   barnesHut: {
  //     gravitationalConstant: -30000,
  //     centralGravity: 1,
  //     springLength: 70,
  //     avoidOverlap: 1
  //   },
  //   stabilization: { iterations: 2500 }
  // },
  interaction: {
    hover: true,
    hoverConnectedEdges: true,
    hoverEdges: true,
    selectable: false,
    selectConnectedEdges: false,
    zoomView: true,
    dragView: true,
    zoomSpeed: 0.5,
  }
};

function App() {

  let data = initialGraph;

  console.log(data)

  let nodesArr = []

  let edgesArr = []

  Object.keys(data).forEach( key => {
    if (nodesArr.filter( n => n.id === key).length > 0 ) {
      return;
    }
    nodesArr.push({
      id: key,
      label: key,
      title: data[key].name,
      department: data[key].department,
      color: 'blue',
    })
    if ("prereqs" in data[key]) {
      if ("AND" in data[key].prereqs) {
        if (nodesArr.filter( n => n.id === key + "_AND").length > 0 ) {
          return;
        }
        nodesArr.push({
          id: key + "_AND",
          label: 'PREREQ OF',
          title: key + "_AND",
          department: data[key].department,
          color: 'blue',
        })
        edgesArr.push({
          from: key + "_AND",
          to: key,
          color: '#D3D3D3',
          department: data[key].department,
        })
        data[key].prereqs.AND.forEach( courseId => {
          if (typeof courseId === 'string') {
            edgesArr.push({
              from: courseId,
              to: key + "_AND",
              color: '#D3D3D3',
              department: data[key].department,
            })
          }
          if (typeof courseId === 'object' && 'OR' in courseId && courseId.OR.length > 0) {
            if (nodesArr.filter( n => n.id === key + "_OR").length > 0 ) {
              return;
            }
            nodesArr.push({
              id: key + "_OR",
              label: 'TAKE EITHER',
              title: key + "_OR",
              department: data[key].department,
              color: 'blue',
            })
            edgesArr.push({
              from: key + "_OR",
              to: key + "_AND",
              color: '#D3D3D3',
              department: data[key].department,
            })
            courseId.OR.forEach( orCourseId => {
              edgesArr.push({
                from: courseId,
                to: key + "_OR",
                color: '#D3D3D3',
                department: data[key].department,
              })
            })
          }
        })
      }
      if ("OR" in data[key].prereqs) {
        if (nodesArr.filter( n => n.id === key + "_OR").length > 0 ) {
          return;
        }
        nodesArr.push({
          id: key + "_OR",
          label: 'TAKE EITHER',
          title: key + "_OR",
          department: data[key].department,
          color: 'blue',
        })
        edgesArr.push({
          from: key + "_OR",
          to: key,
          color: '#D3D3D3',
          department: data[key].department,
        })
        data[key].prereqs.OR.forEach( courseId => {
          if (typeof courseId === 'string') {
            edgesArr.push({
              from: courseId,
              to: key + "_OR",
              color: '#D3D3D3',
              department: data[key].department,
            })
          }
        })
      }
    }
  })

  let departments = nodesArr.map(node => {
    return node.department
  })

  departments = [...new Set(departments)]


  const filteredTerms = ['CIS']
  const newGraph = {
    nodes: nodesArr.filter(node => filteredTerms.some( term => node.department === term)),
    edges: edgesArr.filter(edge => filteredTerms.some( term => edge.department === term))
  };

  const [graph, setGraph] = useState({
    graph: newGraph,
    style: { width: "100%", height: "100%" },
    network: null
  })

  const getNetwork = data => {
    setGraph(prev => {
      let prevObj = Object.assign({}, prev);
      prevObj.network = data;
      return prevObj;
    })
  }

  const getEdges = data => {
    console.log(data)
  }
  const getNodes = data => {
    console.log(data)
  }

  const findCourseSequence = (graph) => {
    findCourseSequence()
  }

  const neighbourhoodHighlight = (params, searchData) => {
    let allNodes = graph.graph.nodes;
    // let allEdges = graph.graph.edges;
    // let cloneNodes = allNodes.map(a => {return {...a}});
    let cloneNodes = allNodes.map(a => {return {...a}});
    // let cloneEdges = allEdges.map(a => {return {...a}});
    let selectedNode = params.node;
    
    cloneNodes.forEach( node => {
      if (node.id === selectedNode) {
        node.color = 'red'
      }
    })

    let connectedNodes = graph.network.getConnectedNodes(selectedNode);
    // let connectedEdges = graph.network.getConnectedEdges(selectedNode);

    for (var node of connectedNodes) {
      connectedNodes = connectedNodes.concat(
        graph.network.getConnectedNodes(node)
      )
      // for (var n1 of graph.network.getConnectedNodes(node)) {
      //   connectedNodes = connectedNodes.concat(
      //     graph.network.getConnectedNodes(n1)
      //   )
      //   for (var n2 of graph.network.getConnectedNodes(n1)) {
      //     connectedNodes = connectedNodes.concat(
      //       graph.network.getConnectedNodes(n2)
      //     )
      //   }
      // }
      // connectedEdges = connectedEdges.concat(
      //   graph.network.getConnectedEdges(node)
      // )
    }

    cloneNodes.forEach( node => {
      if (connectedNodes.some(name => node.id.includes(name))) {
        node.color = 'red'
      } else {
        if (connectedNodes.length > 1) {
          node.color = 'gray'
        }
      }
    })

    // graph.graph.edges.forEach( edge => {
    //   if (connectedEdges.some(name => edge.id.includes(name))) {
    //     console.log(edge)
    //     edge.color = 'red'
    //   }
    // })



    let newGraph = {
      nodes: cloneNodes,
      edges: graph.graph.edges
    }
    setGraph({
      graph: newGraph,
      style: { width: "100%", height: "100%" },
      network: graph.network
    })
  }

  const resetGraphColor = () => {
    let allNodes = graph.graph.nodes;
    // let cloneNodes = allNodes.map(a => {return {...a}});
    let Nodes = allNodes
    let cloneNodes = allNodes.map(a => {return {...a}});

    cloneNodes.forEach( node => {
      node.color = 'blue';
    })

    let newGraph = {
      nodes: cloneNodes,
      edges: graph.graph.edges
    }
    setGraph({
      graph: newGraph,
      style: { width: "100%", height: "100%" },
      network: graph.network
    })
  }

  const events = {
    select: event => {
      var { node, edges } = event;
    },
    hoverNode: event => {
      neighbourhoodHighlight(event, {})
    },
    blurNode: event => {
      resetGraphColor()
    },
    hoverEdge: event => {
      let selectedEdge = graph.graph.edges.filter(edge => edge.id === event.edge)[0]
      neighbourhoodHighlight({ node: selectedEdge.from }, {})
      neighbourhoodHighlight({ node: selectedEdge.to }, {})
    },
    blurEdge: event => {
      resetGraphColor()
    }
  }

  const [filterOption, setFilterOption] = useState(['Computer and Information Science (CIS)']);
  const [nodeNumber, setNodeNumer] = useState(0);
  const [edgeNumber, setEdgeNumber] = useState(0);

  useEffect(() => {
    const newGraph = {
      nodes: nodesArr.filter(node => filterOption.some( term => node.department.includes(term))),
      edges: edgesArr.filter(edge => filterOption.some( term => edge.department.includes(term)))
    };

    setNodeNumer(nodesArr.filter(node => filterOption.some( term => node.department.includes(term))).length)
    setEdgeNumber(edgesArr.filter(edge => filterOption.some( term => edge.department.includes(term))).length)

    setGraph(prev => {
      let prevObj = Object.assign({}, prev);
      prevObj.graph = newGraph;
      return prevObj;
    })

  }, [filterOption])

  window.addEventListener('resize', () => {
    if (graph.network !== null) {
      graph.network.redraw()
      graph.network.fit()
    }
  })

  const handleChange = (event) => {
    const {
      target: { value },
    } = event;
    setFilterOption(
      // On autofill we get a stringified value.
      typeof value === 'string' ? value.split(',') : value,
    );
  }

  const ITEM_HEIGHT = 48;
  const ITEM_PADDING_TOP = 8;
  const MenuProps = {
    PaperProps: {
      style: {
        maxHeight: ITEM_HEIGHT * 10 + ITEM_PADDING_TOP,
      },
    },
  };

  const theme = createTheme({
    palette: {
      primary: {
        main: '#0000FF'
      }
    }
  })

  return (
    <div className="App">
      <ThemeProvider theme={theme}>
        <div
        style={{
          width: '100vw',
          height: '100vh',
          position: 'fixed'
        }}
        >
          <Graph
            graph={graph.graph}
            style={graph.style}
            options={options}
            getNetwork={getNetwork}
            getEdges={getEdges}
            getNodes={getNodes}
            events={events}
          />
        </div>
        <Box
        sx={{
          position: 'fixed',
          top: '20px',
          left: '30px',
          fontSize: '20px',
          border: '1px solid blue',
          borderRadius: '10px',
          p: '10px 12px',
          background: 'white',
        }}
        >
          Penn Course Graph
        </Box>
        <Box
        sx={{
          position: 'fixed',
          top: '20px',
          right: '30px',
          fontSize: '20px',
          border: '1px solid blue',
          borderRadius: '10px',
          p: '10px 12px',
          background: 'white',
        }}
        >
          <Typography
            sx={{
              ml: 1.5,
              textAlign: 'left',
              color: 'blue',
              fontSize: '16px'
            }}
          >
            Display graph for the desired departments
          </Typography>
          <Box sx={{ m: 1 }} />
          <FormControl sx={{ m: 1, width: 'fit-content', maxWidth: '800px' }}>
          <InputLabel id="demo-multiple-checkbox-label">Departments</InputLabel>
          <Select
            labelId="demo-multiple-checkbox-label"
            id="demo-multiple-checkbox"
            multiple
            value={filterOption}
            onChange={handleChange}
            input={<OutlinedInput label="Departments" />}
            renderValue={(selected) => selected.join(', ')}
            MenuProps={MenuProps}
          >
            {departments.map((name) => (
              <MenuItem key={name} value={name}>
                <Checkbox checked={filterOption.indexOf(name) > -1} />
                <ListItemText primary={name} />
              </MenuItem>
            ))}
          </Select>
        </FormControl>
        <Box sx={{m: 1}} />
        <Typography
          sx={{
            textAlign: 'left',
            ml: 1.5,
            color: 'blue',
            fontSize: '16px'
          }}
        >
          Graph Statistics
        </Typography>
        <Box sx={{ m: 1 }} />
        <Stack
          sx={{
            ml: 1,
          }}
          direction='row' 
          spacing={2}>
            <Chip color='primary' label={"Num. Filters: " + filterOption.length} />
            <Chip color='primary' label={"Num. Courses: " + nodeNumber} />
            <Chip color='primary' label={"Num. Edges: " + edgeNumber} />
        </Stack>
        </Box>
      </ThemeProvider>
    </div>
  );
}

export default App;
