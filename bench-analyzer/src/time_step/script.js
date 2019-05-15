// Our labels along the x-axis
var time = [6,12,18,24,30,36,42,48,54,60];
// For drawing the lines
var s1 = [0,0,33,97,141,171,194,208,221,229];
var s2 = [167,172,175,176,177,177,180,180,180,181];
var s3 = [210,213,216,217,218,218,219,219,219,222];
var s4 = [332,332,334,338,342,343,343,344,344,347];
//var s5 = [6,3,2,2,7,26,82,172,312,433,433];

var ctx = document.getElementById("myChart");
var myChart = new Chart(ctx, {
  type: 'line',
  data: {
    labels: time,
    datasets: [
      { 
        data: s1,
        label: "Absolute-03 with Octagon(MSLF,all)",
        borderColor: "#3e95cd",
		fill: false
      },
{ 
  data: s2,
  label: "Absolute-03 with Octagon(Min_max,LB)",
  borderColor: "#8e5ea2",
  fill: false
},
{ 
  data: s3,
  label: "Absolute-03 with Octagon(Max_min, LB)",
  borderColor: "#3cba9f",
  fill: false
},
{ 
  data: s4,
  label: "Gecode with rcpsp-cumulative-LB",
  borderColor: "#e8c3b9",
  fill: false
}//,
//{ 
  //data: northAmerica,
  //label: "North America",
  //borderColor: "#c45850",
  //fill: false
//}
    ]
  }
});
