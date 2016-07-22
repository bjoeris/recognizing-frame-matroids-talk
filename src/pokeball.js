
var width = 600;
var height = 600;
var radius = height / 2 - 5,
    scale = radius;

var svg = d3.select("#vis").append("svg")
  .attr("width", width)
  .attr("height", width);

var rotation = [-15,100,0];
var backRotation = 
  [ -rotation[0]
  , -rotation[1]
  , -rotation[2]
  ]

var fixedProjection = d3.geoOrthographic()
    .translate([width / 2, height / 2])
    .scale(scale)
    .clipAngle(90);
    
var projection = d3.geoOrthographic()
    .translate([width / 2, height / 2])
    .scale(scale)
    .clipAngle(90)
    .rotate(rotation);
    
var backProjection = d3.geoOrthographic()
    .translate([width / 2, height / 2])
    .scale(scale)
    .clipAngle(180)
    .rotate(rotation);
    
var buttonRadius = 10;
var vertexRadius = 2;
var pathGen = d3.geoPath().projection(projection);
var fixedPathGen = d3.geoPath().projection(fixedProjection);
var backPathGen = d3.geoPath().projection(backProjection);
var graticule = d3.geoGraticule()();
var sphere = {type: "Sphere"};
var face1 = d3.geoCircle()();
var face2 = d3.geoCircle().center([180,0])();
var face3 = d3.geoCircle()
  .radius(buttonRadius)
  .center([0,-90])();
var path1 = 
  { type: "MultiLineString"
  , coordinates: [
    [ [90,-90+buttonRadius], 
      [0,90], 
      [-90,-90+buttonRadius]]
  ]};
var paths23 = 
  { type: "MultiLineString"
  , coordinates: face3.coordinates
  };
var vertex1 = d3.geoCircle()
  .radius(vertexRadius)
  .center([90,-90+buttonRadius])();
var vertex2 = d3.geoCircle()
  .radius(vertexRadius)
  .center([-90,-90+buttonRadius])();

svg.append("path")
  .attr("d", pathGen(face1))
  .attr("fill","red")
  .attr("stroke","none");

svg.append("path")
  .attr("d", pathGen(face2))
  .attr("fill","white")
  .attr("stroke","none");
  
svg.append("path")
  .attr("d", pathGen(face3))
  .attr("fill","white")
  .attr("stroke","none");

svg.append("path")
  .attr("d", backPathGen(path1))
  .attr("fill","none")
  .attr("stroke","#ccc")
  .attr("stroke-width","2");
  
svg.append("path")
  .attr("d", pathGen(path1))
  .attr("fill","none")
  .attr("stroke","black")
  .attr("stroke-width","4");
  
svg.append("path")
  .attr("d", pathGen(paths23))
  .attr("fill","none")
  .attr("stroke","black")
  .attr("stroke-width","4");
  
svg.append("path")
  .attr("d", pathGen(vertex1))
  .attr("fill","black");
  
svg.append("path")
  .attr("d", pathGen(vertex2))
  .attr("fill","black");
  
svg.append("path")
  .attr("d", fixedPathGen(graticule))
  .attr("fill","none")
  .attr("stroke","black")
  .attr("stroke-width","0.1");
  
svg.append("path")
  .attr("d", pathGen(sphere))
  .attr("fill","none")
  .attr("stroke","black")
  .attr("stroke-width","0.1");
  