'use strict';
window.onload = init;

function init() {

const mapElement = document.getElementById("mapid");

var box = [-117.06278, 45.22677, -114.29213, 48.99998];

// define rectangle geographical bounds
var bounds = [[box[1], box[0]], [box[3], box[2]]];


const OpenStreetMap_Mapnik = L.tileLayer('https://tile.openstreetmap.org/{z}/{x}/{y}.png', {
	maxZoom: 19,
	attribution: '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
});


const Esri_WorldImagery = L.tileLayer('https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}', {
	attribution: 'Tiles &copy; Esri &mdash; Source: Esri, i-cubed, USDA, USGS, AEX, GeoEye, Getmapping, Aerogrid, IGN, IGP, UPR-EGP, and the GIS User Community'
});

var map = L.map(mapElement, {
layers: [Esri_WorldImagery]
}).fitBounds(bounds);


const baseLayers = {
    'EsriWorldImagery': Esri_WorldImagery,
    'OpenStreetMap': OpenStreetMap_Mapnik
  }


/*
function getColor(d) {
 return d > 0.9 ? "#47039FFF" :
  d  > 0.8 ? "#7301A8FF" :
  d  > 0.7 ? "#9C179EFF" :
  d  > 0.6 ? "#BD3786FF" :
  d  > 0.5  ? "#D8576BFF" :
  d  > 0.4 ? "#ED7953FF" :
  d  > 0.3 ? "#FA9E3BFF" :
  d  > 0.2 ? "#FDC926FF" :
  d > 0.1 ? "#F0F921FF" :
             "#6c9ad5" ;

}
*/

function getColor(d) {
 switch(d){

  case 'fa': return "#228b22";
  case 'far': return "#E69F00";
 case 'fur': return "#b22222";
 default: return  "ffffff";
}
}


function style(feature) {
    return {
        fillColor: getColor(feature.properties.final_risk),
        weight: 0.25,
        opacity: 1,
        color: 'white',
        fillOpacity: 0.7
    };
}


var popupFunc = function(feature) {

  return  '<strong><center><div style="font-size:14px;">Summary Table</div></center></strong>'+
          '<div style="font-size:11px;"><b>Final Risk: </b>' + feature.properties.final_risk + '<div class="box" style="background-color:' + getColor(feature.properties.final_risk) + '"></div> ' +
          '</strong><br><b>Proportion: </b>' + String(Math.round((Number(feature.properties.proportion))*100)/100) +
          '</strong><br><b>FA: </b>' + String(Math.round((Number(feature.properties.fa))*100)/100) +
          '</strong><br><b>FAR: </b>' + String(Math.round((Number(feature.properties.far))*100)/100) +
          '</strong><br><b>FUR: </b>' + String(Math.round((Number(feature.properties.fur))*100)/100) +
          '</strong><br><b>Specific Sediment Delivery: </b>' + String(Math.round((Number(feature.properties.spec_delFS))*100)/100) +
          '</strong><br><b>Road Length (mi): </b>' + String(Math.round((Number(feature.properties.road_length))*0.621371*100)/100) +
          '</strong><br><b>HUC 12: </b>' + feature.properties.huc12 +'</div>'

}


/// customTip function

function customTip(layer,feature) {
    layer.unbindTooltip();
    if(!layer.isPopupOpen()) layer.bindTooltip(popupFunc(feature), {className: 'myCSSClass'}).openTooltip();
}

/// Adding the final model layers

$.getJSON("btb_hucs.geojson", function(data) {

  let hucs = L.geoJSON(data,{
  style: style,
  onEachFeature: function onEachFeature(feature, layer) {
                //layer.bindPopup(popupFunc(feature), {className: 'myCSSClass'}).openPopup();

layer.on('mouseover', customTip(layer,feature));

}
  }).addTo(map);

layerControls.addOverlay(hucs, 'Integrated Risk')


});

var markers_pibo = L.markerClusterGroup({
	spiderfyOnMaxZoom: true,
	showCoverageOnHover: true,
	zoomToBoundsOnClick: true
});

/// customTip function

var popupFuncPibo = function(feature) {

  return  '<strong><center><div style="font-size:14px;">' + 'PIBO Site' + '</div></center></strong>' +
          '</strong><div style="font-size:11px;"><b>Stream: </b>' + feature.properties.Stream +
          '</strong><br><b>Forest: </b>' + feature.properties.Forest +
          '</strong><br><b>District: </b>' + feature.properties.District +
          '</strong><br><b>Type: </b>' + feature.properties.Mgmt + '</div>'

}


function customTipPibo(layer,feature) {
    layer.unbindTooltip();
    if(!layer.isPopupOpen()) layer.bindTooltip(popupFuncPibo(feature), {className: 'myCSSClass'}).openTooltip();
}


$.getJSON("pibo.geojson", function(data) {

 let pibo = L.geoJSON(data,{
                onEachFeature: function onEachFeature(feature, layer) {
                //layer.bindPopup(popupFuncPibo(feature), {className: 'myCSSClass'}).openPopup();

                layer.on('mouseover', customTipPibo(layer,feature));

                }

 });

markers_pibo.addLayer(pibo);

layerControls.addOverlay(markers_pibo, 'PIBO Monitoring Sites')

});
/*
var legend = L.control({position: 'bottomright'});

legend.onAdd = function (map) {

    var div = L.DomUtil.create('div', 'info legend'),
        grades = [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9],
        labels = [];

    div.innerHTML += '<strong> Specific Sediment<br> Delivery </strong><br>'

    // loop through our density intervals and generate a label with a colored square for each interval
    for (var i = 0; i < grades.length; i++) {
      div.innerHTML +=
              '<i style="background:' + getColor(grades[i]) + '"></i> '+
          String(Math.round((Number(grades[i])-0.1)*10)/10) + '&ndash;' + grades[i] + '<br>';
    }

    return div;
};

legend.addTo(map);
*/

const layerControls = L.control.layers(baseLayers, {}, {}).addTo(map);

// adding controls
L.Control.Watermark = L.Control.extend({
    onAdd: function(map) {
        var img = L.DomUtil.create('img');

        img.src = 'https://www.fs.usda.gov/themes/custom/fs_uswds/logo.svg';
        img.style.width = '50px';

        return img;
    },

    onRemove: function(map) {
        // Nothing to do here
    }
});

L.control.watermark = function(opts) {
    return new L.Control.Watermark(opts);
}

L.control.watermark({ position: 'bottomleft' }).addTo(map);


L.Control.MyControl = L.Control.extend({
  options: {
    position: 'topright'
  },
  onAdd: function(map) {
    var button = L.DomUtil.create('button', 'open-modal-btn');
    button.innerText='Bayesian Network';
    button.onclick = function() {
      var myModal = new bootstrap.Modal(document.getElementById('myModal'));
      myModal.show();
    }


    return button;

  },

  onRemove: function(map) {
    // Nothing to do here
  }
});

map.addControl(new L.Control.MyControl());

var downloadControl = L.Control.extend({
    options: {
        position: 'topright' // You can change the position based on your needs
    },

    onAdd: function (map) {
        var button = L.DomUtil.create('button', 'open-modal-btn');
    button.innerText='Download Data';

        button.onclick = function(){
            var myModal = new bootstrap.Modal(document.getElementById('downloadModal'));
            myModal.show();
        }

        return button;
    },

  onRemove: function(map) {
    // Nothing to do here
  }

});

map.addControl(new downloadControl());
/*
var SelectorControl = L.Control.extend({
  options: { position: 'topright' // Position of the control
},
onAdd: function (map) {
  var container = L.DomUtil.create('div', 'leaflet-bar leaflet-control leaflet-control-custom'); // Create the select element

var select = L.DomUtil.create('select', 'form-select', container);
                      select.innerHTML = `
                                          <option value="column1">Column 1</option>
                                          <option value="column2">Column 2</option>
                                          <option value="column3">Column 3</option> `;

        // Add change event listener to update the map
        L.DomEvent.on(select, 'change', function (e) {
          var selectedColumn = select.value;
          updateMapData(selectedColumn);

        }); // Prevent map interactions when interacting with the control

        L.DomEvent.disableClickPropagation(container);
        return container;

}

});
map.addControl(new SelectorControl());

*/
var bayesnet = document.getElementById('bayesnet');

bayesnet.onclick = function() {
      var myModal = new bootstrap.Modal(document.getElementById('myModal'));
      myModal.show();
    }

var finalScores = document.getElementById('finalScores');

finalScores.onclick = function() {
      var myScoreModal = new bootstrap.Modal(document.getElementById('finalScoresModal'));
      myScoreModal.show();
    }



}




