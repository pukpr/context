////////////////////////////////////////////////
//  the VisQuick ProtoVis scatter plot interface
////////////////////////////////////////////////
handleLogTick = function(n) {
               var Num = Math.log(n)/Math.log(10);
               var Den = Math.round((Math.log(n)/Math.log(10)));
               var X = 0;
               if(Den==0.0) {
                  if(Num==0.0) X = 1.0;
	       } else {
                  X = Num/Den;
	       }
               // console.log(X);
               if( Math.abs(X-1) < 0.001  ) {
	           return (n.toPrecision(1));
	       } else {
		   return ('');
	       }
	     }

handleTick = function(n) {
               return (n.toPrecision(2));
	     }

handleShape = function(x) {
               // console.log(x)
               // var obj = jQuery.parseJSON(x);
               //console.log(x.id[0]);
               switch(x.id[0]) {
	       case 't' : return "triangle";
	       case 'x' : return "cross";
	       case 'o' : return "circle";
	       case 's' : return "square";
	       case 'd' : return "diamond";
               default : return "circle";
	       }
	     }


handleScatterData = function(LogScale,S,P,data_array){   // data_array is JSON-like strings
  var sp = new vq.ScatterPlot();
  var HT = handleTick;
  if(LogScale==1) HT = handleLogTick;
  var data =
  {
   DATATYPE : "vq.models.ScatterPlotData",
   CONTENTS : {
               PLOT : {
                       container : document.getElementById("masterContainer"),
                       width  : 700,
                       height : 400,
                       vertical_padding   : 40,
                       horizontal_padding : 80,
                       font : "36px sans-serif"  // This doesn't seem to work
                      },
               data_array    : data_array,
               xcolumnid     : S,
               ycolumnid     : P,
	       logScale : LogScale,
               x_axis_tick_format : HT,
	       y_axis_tick_format : HT,
               shape : handleShape,
               valuecolumnid : 'id'
              }
  };
  sp.draw(data);
};


////////////////////////////////////////////////
//  the Google Line Chart (corechart) interface
////////////////////////////////////////////////

handleChartData = function(data_array){  // data_array is Prolog-like lists using strings

  var data = google.visualization.arrayToDataTable(data_array);
  var options = {
    title: 'Profile'
  };
  var chart = new google.visualization.LineChart(document.getElementById('chart_div'));
  chart.draw(data, options);
  var slider = new google.visualization.ControlWrapper({
      'controlType': 'NumberRangeFilter',
      'containerId': 'slider_div',
      'options': {
        'filterColumnIndex': 2,
        'ui': {
          'labelStacking': 'vertical',
          'label': 'Profile:'
        }
      }
  });
};



////////////////////////////////////////////////
//  the DynaGraph scatter plot interface
////////////////////////////////////////////////

handleDygraphData = function(logselector, Xlabel, Ylabel, Title, data_array){  // data_array is CSV with embedded \n line-breaks
 g = new Dygraph(  // containing div
                   document.getElementById("graphdiv"),
                   data_array,
                   {
                      rollPeriod: 1,
                      showRoller: true,
                      title: Title,
                      logscale: logselector,
		      ylabel: Ylabel,
		      xlabel: Xlabel,
		      yAxisLabelWidth: 70,
                      labelsSeparateLines: true,
                      labelsDivWidth: 150,
                      labelsDivStyles: {'textAlign': 'right'}
                   }
                 );

// CSV or path to a CSV file.
//    "Date,Temperature\n" +
//    "2008-05-07,75\n" +
//    "2008-05-08,70\n" +
//    "2008-05-09,80\n"
//  );

};

handleDygraphList = function(logselector, Xlabel, Ylabel, Title, Labels, data_array){  // data_array is a list of tuples
 g = new Dygraph(  // containing div
                   document.getElementById("graphdiv"),
                   data_array,
                   {
                      rollPeriod: 1,
                      showRoller: true,
                      title: Title,
                      logscale: logselector,
		      digitsAfterDecimal: 4,
		      ylabel: Ylabel,
		      xlabel: Xlabel,
		      yAxisLabelWidth: 70,
		      labels: Labels,
                      labelsSeparateLines: true,
                      labelsDivWidth: 150,
                      labelsDivStyles: {'textAlign': 'right'}
                   }
                 );

// List
//    [
//     [x,y,z],
//     [x,y,z]
//    ]
//  );

};


handleDygraphListErrorBars = function(logselector, Xlabel, Ylabel, Title, Labels, data_array){  // data_array is a list of tuples
 g = new Dygraph(  // containing div
                   document.getElementById("graphdiv"),
                   data_array,
                   {
                      rollPeriod: 1,
                      showRoller: true,
                      title: Title,
                      errorBars: true,
                      logscale: logselector,
		      ylabel: Ylabel,
		      xlabel: Xlabel,
		      yAxisLabelWidth: 70,
		      labels: Labels,
                      labelsSeparateLines: true,
                      labelsDivWidth: 150,
                      labelsDivStyles: {'textAlign': 'right'}
                   }
                 );

// List
//    [
//     [x,y,z],
//     [x,y,z]
//    ]
//  );

};


////////////////////////////////////////////////
//  the Google Line Chart with slider interface
////////////////////////////////////////////////

drawVisualization = function(Log, Xaxis, Yaxis, Title, data_array) {
  var data = google.visualization.arrayToDataTable(data_array);


  // Define a NumberRangeFilter slider control for the 'Z' column.
  var slider = new google.visualization.ControlWrapper({
    'controlType': 'NumberRangeFilter',
    'containerId': 'slider_div',
    'options': {
      'filterColumnLabel': 'X'
      //,
      //'minValue': -10,
      //'maxValue': 10
    }
  });

  // Define a line chart
  var lineChart = new google.visualization.ChartWrapper({
    'chartType': 'LineChart',
    'containerId': 'chart_div',
    //,
    'options': {
      'title': Title,
      'hAxis': {
         'title': Xaxis,
         'logScale': Log
	       },
      'vAxis': {
         'title': Yaxis,
         'logScale': Log
	       }
    //  'width': 400,
    //  'height': 300,
    //  'hAxis': {'minValue': -60, 'maxValue': 60},
    //  'chartArea': {top: 0, right: 0, bottom: 0}
    }
  });

  // Create the dashboard.
  var dashboard = new google.visualization.Dashboard(document.getElementById('dashboard'))
    // Configure the slider to affect the line chart
      .bind(slider, lineChart)
    // Draw the dashboard
      .draw(data);
}

getSliderLo = function() {
   return document.getElementById("slider_div").getElementsByClassName("google-visualization-controls-rangefilter-thumblabel")[0].textContent;
}

getSliderHi = function() {
   return document.getElementById("slider_div").getElementsByClassName("google-visualization-controls-rangefilter-thumblabel")[1].textContent;
}

drawMap = function(Lat, Lon, Title) {
   var myLatlng = new google.maps.LatLng(Lat,Lon);
   var mapOptions = {
       zoom: 4,
       center: myLatlng,
       mapTypeId: google.maps.MapTypeId.ROADMAP
    }
   var map = new google.maps.Map(document.getElementById('map_canvas'),
				 mapOptions);

   var marker = new google.maps.Marker({
          position: map.getCenter(),
          map: map,
          title: Title
        });


//   google.maps.event.addListener(map, 'center_changed', function() {
//        // 30 seconds after the center of the map has changed, pan back to marker.
//          window.setTimeout(function() {
//            map.panTo(marker.getPosition());
//          }, 30000);
//        });


   google.maps.event.addListener(marker, 'click', function() {
          map.setZoom(16);
          map.setCenter(marker.getPosition());
        });

   var bounds = new google.maps.LatLngBounds(
	    myLatlng,
	    new google.maps.LatLng(Lat+1.0,Lon+1.0));

   rectangle = new google.maps.Rectangle({
          strokeColor: "#00FF00",
          strokeOpacity: 1.0,
          strokeWeight: 2,
          fillColor: "#0000FF",
          fillOpacity: 0.05,
	  map: map,
          bounds: bounds
        });
}

// Try at retrieving values via JavaScript, better to use Google Web URI API

getElevation = function(Lat, Lon) {
  var myLatlng = new google.maps.LatLng(Lat,Lon);
  elevator = new google.maps.ElevationService();
  var locations = [];
  locations.push(myLatlng);
  var positionalRequest = {
    'locations': locations
  };
  // Initiate the location request
  elevator.getElevationForLocations(
    positionalRequest,
    function(results, status) {
       if (status == google.maps.ElevationStatus.OK) {
          // Retrieve the first result
          if (results[0]) {
             var Elev = results[0].elevation;
             // alert(Elev);
	     return(Elev);
          } else {
             alert("No results found");
          }
      } else {
         alert("Elevation service failed due to: " + status);
      }
    }
 )
}
