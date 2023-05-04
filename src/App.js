import './App.css';
import Graph from 'vis-react';
import initialGraph from "./data.json";
import React, { useEffect, useState, useRef } from 'react';
import { Box, Typography, Chip, Stack, TextField, IconButton, Switch, Modal, ListItem } from '@mui/material';
import OutlinedInput from '@mui/material/OutlinedInput';
import InputLabel from '@mui/material/InputLabel';
import MenuItem from '@mui/material/MenuItem';
import FormControl from '@mui/material/FormControl';
import ListItemText from '@mui/material/ListItemText';
import Select, { SelectChangeEvent } from '@mui/material/Select';
import Checkbox from '@mui/material/Checkbox';
import { createTheme, ThemeProvider } from '@mui/material/styles';
import CircleIcon from '@mui/icons-material/Circle';
import SendIcon from '@mui/icons-material/Send';
import CustomizedSnackbars from './CustomizedSnackbar';
import model from 'wink-eng-lite-web-model';
import similarity from 'wink-nlp/utilities/similarity';
import winkNLP from 'wink-nlp'
import { FixedSizeList } from 'react-window';
import Accordion from '@mui/material/Accordion';
import AccordionSummary from '@mui/material/AccordionSummary';
import AccordionDetails from '@mui/material/AccordionDetails';
import ExpandMoreIcon from '@mui/icons-material/ExpandMore';


const nlp = winkNLP( model );
const its = nlp.its;
const as = nlp.as;

const style = {
  position: 'absolute',
  top: '50%',
  left: '50%',
  transform: 'translate(-50%, -50%)',
  width: 600,
  bgcolor: 'background.paper',
  border: '1px solid blue',
  boxShadow: 1,
  p: 4,
}

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
    size: 15,
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
    width: 0.5,
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
  // physics: {
  //   enabled: true
  // },
  physics: {
    barnesHut: {
      gravitationalConstant: -30000,
      centralGravity: 1,
      springLength: 70,
      avoidOverlap: 3
    },
    stabilization: { iterations: 50000 }
  },
  interaction: {
    hover: true,
    hoverConnectedEdges: true,
    hoverEdges: true,
    selectable: false,
    selectConnectedEdges: false,
    zoomView: true,
    dragView: true,
    zoomSpeed: 0.5,
    navigationButtons: true
  }
};

let nodesArr = []

let edgesArr = []

let data = initialGraph;

Object.keys(data).forEach( key => {
  // data[key].embedding = nlp.readDoc(data[key].description).tokens().out(its.value, as.bow)
  if (nodesArr.filter( n => n.id === key).length > 0 ) {
    return;
  }
  nodesArr.push({
    id: key,
    label: key,
    title: data[key].name,
    department: data[key].department,
    description: data[key].description,
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
        type: 'AND'
      })
      data[key].prereqs.AND.forEach( courseId => {
        if (typeof courseId === 'string') {
          edgesArr.push({
            from: courseId,
            to: key + "_AND",
            color: '#D3D3D3',
            department: data[key].department,
            type: 'AND'
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
            type: 'OR_AND'
          })
          courseId.OR.forEach( orCourseId => {
            edgesArr.push({
              from: courseId,
              to: key + "_OR",
              color: '#D3D3D3',
              department: data[key].department,
              type: 'OR',
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
        type: 'OR'
      })
      data[key].prereqs.OR.forEach( courseId => {
        if (typeof courseId === 'string') {
          edgesArr.push({
            from: courseId,
            to: key + "_OR",
            color: '#D3D3D3',
            department: data[key].department,
            type: 'OR'
          })
        }
      })
    }
  }

  if ("similar" in data[key] && data[key]["similar"].length > 0) {
    if (nodesArr.filter( n => n.id === key + "_SIMILAR").length > 0 ) {
      return;
    }
    nodesArr.push({
      id: key + "_SIMILAR",
      label: 'SIMILAR TO',
      title: key + "_SIMILAR",
      department: data[key].department,
      color: 'blue'
    })
    edgesArr.push({
      from: key,
      to:  key + "_SIMILAR",
      type: 'SIMILAR',
      color: '#D3D3D3',
      department: data[key].department,
    })
    data[key].similar.forEach(node => {
      edgesArr.push({
        from: key + "_SIMILAR",
        to: node,
        type: 'SIMILAR',
        color: '#D3D3D3',
        department: data[key].department,
      })
    })
  }
})

let departments = nodesArr.map(node => {
  return node.department
})

departments = [...new Set(departments)].sort()


let department_colors = {}

departments.map(department => {
  var randomColor = '#' +  Math.floor(Math.random()*16777215).toString(16);
  department_colors[department] = randomColor;
})

nodesArr.forEach(node => {
  node.color = department_colors[node.department]
})

const filteredTerms = ['CIS']
const newGraph = {
  nodes: nodesArr.filter(node => filteredTerms.some( term => node.department === term)),
  edges: edgesArr.filter(edge => filteredTerms.some( term => edge.department === term))
};

export const useIsMount = () => {
  const isMountRef = useRef(true);
  useEffect(() => {
    isMountRef.current = false;
  }, []);
  return isMountRef.current;
};

let renderCount = 0

function App() {

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
      node.color = department_colors[node.department]
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
      if (selectedEdge !== undefined) {
        neighbourhoodHighlight({ node: selectedEdge.from }, {})
        neighbourhoodHighlight({ node: selectedEdge.to }, {})
      }
    },
    blurEdge: event => {
      resetGraphColor()
    }
  }

  const [filterOption, setFilterOption] = useState(['Computer and Information Science (CIS)']);
  const [nodeNumber, setNodeNumer] = useState(0);
  const [edgeNumber, setEdgeNumber] = useState(0);

  const [prereqEdges, setPrereqEdges] = useState(true)

  const [similarityEdges, setSimilarityEdges] = useState(false)

  const handlePrereqSwitch = event => {
    setPrereqEdges(event.target.checked)
  }

  const handleSimilaritySwitch = event => {
    setSimilarityEdges(event.target.checked)
  }

  const isMount = useIsMount();

  const checkGraph = () => {
    if (graph == undefined || isMount) {
      return;
    }
  }

  const checkGraphBefore = (graph) => {
    if (graph == undefined || isMount) {
      return;
    }
    if (prereqEdges) {
      console.log('PRE')
      const filters = ['AND', 'OR', 'OR_AND']
      let edges = graph.graph.edges
      graph.network.body.data.edges._data = {}
      edges.forEach(edge => {
        if (filters.includes(edge.type)) {
          graph.network.body.data.edges.add(edge)
        }
      })
      // let edges = Object.keys(graph.network.body.data.edges._data);
      // edges.forEach(edge => {
      //   let e = graph.network.body.data.edges._data[edge]
      //   if (filters.includes(e.type)) {
      //     graph.network.body.data.edges.remove(edge)
      //   }
      // })
      
    } else if (!prereqEdges) {
      console.log('NOPRE')
      const filters = ['AND', 'OR', 'OR_AND']
      let edges = Object.keys(graph.network.body.data.edges._data);
      edges.forEach(edge => {
        let e = graph.network.body.data.edges._data[edge]
        if (filters.includes(e.type)) {
          graph.network.body.data.edges.remove(edge)
        }
      })
    }
    if (similarityEdges) {
      console.log('heee')
      const filters = ['SIMILAR']
      let edges = graph.graph.edges
      graph.network.body.data.edges._data = {}
      edges.forEach(edge => {
        if (filters.includes(edge.type)) {
          graph.network.body.data.edges.add(edge)
        }
      })
    } else if (!similarityEdges) {
      console.log('hoooo')
      const filters = ['SIMILAR']
      let edges = Object.keys(graph.network.body.data.edges._data);
      edges.forEach(edge => {
        let e = graph.network.body.data.edges._data[edge]
        if (filters.includes(e.type)) {
          graph.network.body.data.edges.remove(edge)
        }
      })
    }
  }

  // useEffect(() => {
  //   renderCount = renderCount + 1
  //   checkGraph();
  // }, [prereqEdges, similarityEdges, filterOption])

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

  const handleSubmit = event => {
    event.preventDefault()
    console.log(event)
  }

  const handleSequenceChange = event => {
    console.log(event)
  }

  // useEffect(() => {
  //   if (graph === undefined) {
  //     return;
  //   }
  //   const filters = ['AND', 'OR', 'OR_AND']
  //   if (prereqEdges) {
  //     console.log(graph.graph.edges)
  //     // let edges = Object.keys(graph.network.body.data.edges._data);
  //     // edges.forEach(edge => {
  //     //   let e = graph.network.body.data.edges._data[edge]
  //     //   if (filters.includes(e.type)) {
  //     //     graph.network.body.data.edges.remove(edge)
  //     //   }
  //     // })
      
  //   } else {
  //     let edges = Object.keys(graph.network.body.data.edges._data);
  //     edges.forEach(edge => {
  //       let e = graph.network.body.data.edges._data[edge]
  //       if (filters.includes(e.type)) {
  //         graph.network.body.data.edges.remove(edge)
  //       }
  //     })
  //   }
  // }, [prereqEdges, filterOption])


  const [searchQuery, SetSearchQuery] = useState('');

  const [searchData, setSearchData] = useState([])

  const handleSearchChange = (event) => {
    SetSearchQuery(event.target.value)
  }

  const [openModal, setOpenModal] = useState(false)
  
  const handleOpenModal = () => {
    setOpenModal(true)
  }

  const handleCloseModal = () => {
    setOpenModal(false)
  }

  const handleSearchSubmit = () => {
    const origin = nlp.readDoc(searchQuery).tokens().out(its.value, as.bow)
    const courses = Object.keys(data)
    let bestCourses = []
    courses.forEach(course => {
      const score = similarity.bow.cosine(origin, data[course].embedding)
      data[course].score = score
      bestCourses.push(data[course])
      data[course].similar_courses = []
      data[course].similar.forEach(sim => {
        data[course].similar_courses.push(data[sim])
      })
    })
    bestCourses.sort((a, b) => (a.score > b.score) ? -1 : 1)
    setSearchData(bestCourses.slice(0, 5))
    console.log(bestCourses.slice(0, 5))
    handleOpenModal()
  }

  function renderRow(props) {
    const { index, style } = props;
  
    return (
      <>
        {searchData.map(data => (
          <ListItem style={style} key={index} component="div">
            <ListItemText>{data.code + " " + data.name}</ListItemText>
            <ListItemText>{data.description}</ListItemText>
          </ListItem>
        ))}
      </>
    );
  }

  

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
          alignItems: 'start',
          display: 'flex',
          flexDirection: 'column'
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
                <CircleIcon 
                  sx={{
                    color: department_colors[name]
                  }}
                />
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
        <Box sx={{ m: 2 }} />
        <Box
          component='form'
          autoComplete='off'
          sx={{
            textAlign: 'left'
          }}
        >
          <Typography
            sx={{
              color: 'blue',
              fontSize: '16px',
              ml: 1.5,
            }}
          >
            Recommend Courses
          </Typography>
          <Box sx={{ m: 1 }} />
          <Box
            component='form'
            sx={{
              display: 'flex',
              ml: 1.5,
              minWidth: '350px'
            }}
          >
            <TextField 
              sx={{ width: '100%' }} 
              label="I want to take course about ..." 
              variant="outlined" 
              preventDefault={true}
              onChange={handleSearchChange}
              onKeyPress={e => e.key === 'Enter' && e.preventDefault()}
            />
             <Box sx={{ m: 1 }} />
              <IconButton
              onClick={handleSearchSubmit}
              >
                <SendIcon sx={{ color: 'blue' }} />
              </IconButton>
          </Box>
        </Box>
        <Box sx={{ m: 1 }} />
        <Box sx={{ ml: 1.5 }}>
          {/* <Stack direction='row'>
            <Stack
              direction='row'
              sx={{
                alignItems: 'center'
              }}
            >
              <Typography
                sx={{
                  color: 'blue'
                }}
              >
                Prerequisite Edges
              </Typography>
              <Switch onChange={handlePrereqSwitch} checked={prereqEdges} label="Prerequisite Edges" />
            </Stack>
            <Stack
              direction='row'
              sx={{
                alignItems: 'center'
              }}
            >
              <Typography
                sx={{
                  color: 'blue'
                }}
              >
                Similarity Edges
              </Typography>
              <Switch onChange={handleSimilaritySwitch} checked={similarityEdges} label="Similarity Edges" />
            </Stack>
          </Stack> */}
        </Box>
        </Box>
        <CustomizedSnackbars />
        <Modal
        open={openModal}
        onClose={handleCloseModal}
        aria-labelledby="modal-modal-title"
        aria-describedby="modal-modal-description"
      >
        <Box sx={style}>
          <Typography sx={{
            color: 'blue',
            fontSize: '25px',
            fontWeight: '500',
            borderBottom: '1px solid blue'
          }} id="modal-modal-title" variant="h2" component="h2">
            Recommended Courses
          </Typography>
          <Box sx={{
            height: 400,
            overflowY: 'scroll',
          }}>
            {searchData.map(data => (
              <Box>
                <Box sx={{ m: 1.5 }} />
                <Typography
                sx={{
                  fontSize: '20px',
                  color: 'blue',
                }}
                >{data.code + " " + data.name}</Typography>
                <Box sx={{ m: 0.5}} />
                <Typography>{data.description}</Typography>
                <Box>
                  {
                    data.similar_courses.length > 0 && (
                      <>
                      <Typography
                        sx={{
                          color: 'blue',
                          fontSize: '18px',
                          fontWeight: 500,
                        }}
                        >Similar Courses</Typography>
                        <Box sx={{ m: 1 }} />
                          {data.similar_courses.map(c => (
                            <Accordion
                            expandIcon={
                            <ExpandMoreIcon 
                            sx={{
                              color: 'blue'
                            }}/>}
                            sx={{
                              boxShadow: 'none',
                              borderTop: '1px solid blue'
                            }}
                            >
                                <AccordionSummary>
                                  <Typography sx={{
                                    color: 'blue'
                                  }}>{c.code + " " + c.name}</Typography>
                                </AccordionSummary>
                                <AccordionDetails>
                                  <Typography>
                                    {c.description}
                                  </Typography>
                                </AccordionDetails>
                        
                            </Accordion>
                          ))}
                      </>
                    )
                  }
                  </Box>
              </Box>
              // <ListItem component="div">
              //   <ListItemText>{data.code + " " + data.name}</ListItemText>
              //   <ListItemText>{data.description}</ListItemText>
              // </ListItem>
            ))}
          </Box>
          {/* <ListItem
          height={400}
          >
            {searchData.map(data => (
              <div></div>
            ))}
          </ListItem> */}
        </Box>
      </Modal>
      </ThemeProvider>
    </div>
  );
}

export default App;
