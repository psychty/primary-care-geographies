// https://github.com/Leaflet/Leaflet.heat

// L. is leaflet
var tileUrl = 'https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png';
var attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors, <a href="https://creativecommons.org/licenses/by-sa/2.0/">CC-BY-SA</a>, Contains Ordnance Survey data © Crown copyright and database right 2010-19.';

var color_pcn_group = d3.scaleOrdinal()
    .domain(["West Sussex - Arun PCN (Neighbourhood 1)", "West Sussex - Arun PCN (Neighbourhood 2)", "West Sussex - Burgess Hill and Villages PCN", "West Sussex - Central Worthing PCN" , "West Sussex - Champ PCN" , "West Sussex - Chanctonbury PCN", "West Sussex - Cissbury Integrated Care (CIC) PCN", "West Sussex - Coastal and South Downs Care Partnership PCN", "West Sussex - Crawley Care Collaborative PCN" , "West Sussex - East Grinstead PCN"  , "West Sussex - Haywards Heath Town PCN", "West Sussex - Haywards Heath Villages PCN", "West Sussex - Healthy Crawley PCN", "West Sussex - Horsham Central PCN", "West Sussex - Horsham Collaborative PCN", "West Sussex - Lancing PCN", "West Sussex - Regis PCN (Neighbourhood 1 - Central Regis)", "West Sussex - Regis PCN (Neighbourhood 2 - Rural Regis)", "West Sussex - Rural North Chichester PCN", "West Sussex - Shoreham PCN", "West Sussex - South Crawley PCN"])
    .range(["#a04866", "#d74356", "#c4705e", "#ca572a", "#d49445", "#526dd6", "#37835c", "#a2b068", "#498a36", "#a678e4", "#8944b3", "#57c39b", "#4ab8d2", "#658dce", "#776e29", "#60bf52", "#7e5b9e", "#afb136", "#ce5cc6", "#d58ec6", "#d44b92"]);

    // data_url = "./pcn_overview.json";

    // async function getData(data_url) {
    //   const response = await fetch(data_url);
    //   const data = await response.json();
    //   // console.log(data);
    // }

    // getData("./pcn_overview.json");
    // getData('./gp_lookup_pcn_overview.json')

    var request = new XMLHttpRequest();
    request.open("GET", 'pcn_overview.json', false);
    request.send(null);
    var pcn_overview = JSON.parse(request.responseText);

// Create a function for tabulating the data
  function tabulate_pcn(data, columns) {
  var table = d3.select('#pcn_overview_table')
        .append('table')
  var thead = table
        .append('thead')
  var tbody = table
        .append('tbody');

    // append the header row
    thead
    .append('tr')
    .selectAll('th')
    .data(columns).enter()
    .append('th')
    .text(function (column) {
          return column;
              });

    // create a row for each object in the data
    var rows = tbody.selectAll('tr')
      .data(data)
      .enter()
      .append('tr');

    // create a cell in each row for each column
    var cells = rows.selectAll('td')
      .data(function (row) {
        return columns.map(function (column) {
        return {column: column, value: row[column]};
          });
          })
      .enter()
      .append('td')
      .text(function(d,i) {
        if(i === 1) return d3.format(",.0f")(d.value);
        if(i === 2) return d3.format(",.0f")(d.value);
        if(i === 3) return d3.format(",.0f")(d.value);
        if(i === 4) return d3.format(".1%")(d.value);
                   return d.value; })
        return table;
        }

var topTable = tabulate_pcn(pcn_overview, ['PCN', 'Number of practices', 'Patients', 'Patients aged 65+', 'Proportion aged 65+']);

var request = new XMLHttpRequest();
request.open("GET", 'gp_lookup_pcn_overview.json', false);
request.send(null);
var gp_overview = JSON.parse(request.responseText);

// read PCN
// Add AJAX request for data
var pcn = $.ajax({
  url:"./pcn_simple.geojson",
  dataType: "json",
  success: console.log("PCN boundary data successfully loaded."),
  error: function (xhr) {
    alert(xhr.statusText)
  }
})

// read LAD
// Add AJAX request for data
var lad = $.ajax({
  url:"./lad_simple.geojson",
  dataType: "json",
  success: console.log("LAD boundary data successfully loaded."),
  error: function (xhr) {
    alert(xhr.statusText)
  }
})

function setLADColor(d) {
  return d === 'Adur' ? '#FC4E08' :
    d === 'Arun' ? '#FD8D1A' :
    d === 'Chichester' ? '#FEB22A' :
    d === 'Crawley' ? '#FED954' :
    '#FFED80';
}

function LADColor(feature) {
  return {
    // color: setLADColor(feature.properties.LAD19NM),
    color: '#3D2EFF',
    fill: null,
    weight: 2,
    // dashArray: '3',
    fillOpacity: 0};
}

function setPCNColor(d) {
      return d === 'West Sussex - Arun PCN (Neighbourhood 1)' ? '#a04866' :
      d === 'West Sussex - Arun PCN (Neighbourhood 2)' ? '#d74356' :
      d === 'West Sussex - Burgess Hill and Villages PCN' ? '#c4705e' :
      d === 'West Sussex - Central Worthing PCN' ? '#ca572a' :
      d === 'West Sussex - Champ PCN' ? '#d49445' :
      d === 'West Sussex - Chanctonbury PCN' ? '#526dd6' :
      d === 'West Sussex - Cissbury Integrated Care (CIC) PCN' ? '#37835c' :
      d === 'West Sussex - Coastal and South Downs Care Partnership PCN' ? '#a2b068' :
      d === 'West Sussex - Crawley Care Collaborative PCN' ? '#498a36' :
      d === 'West Sussex - East Grinstead PCN' ? '#a678e4' :
      d === 'West Sussex - Haywards Heath Town PCN' ? '#8944b3' :
      d === 'West Sussex - Haywards Heath Villages PCN' ? '#57c39b' :
      d === 'West Sussex - Healthy Crawley PCN' ? '#4ab8d2' :
      d === 'West Sussex - Horsham Central PCN' ? '#658dce' :
      d === 'West Sussex - Horsham Collaborative PCN' ? '#776e29' :
      d === 'West Sussex - Lancing PCN' ? '#60bf52' :
      d === 'West Sussex - Regis PCN (Neighbourhood 1 - Central Regis)' ? '#7e5b9e' :
      d === 'West Sussex - Regis PCN (Neighbourhood 2 - Rural Regis)' ? '#afb136' :
      d === 'West Sussex - Rural North Chichester PCN' ? '#ce5cc6' :
      d === 'West Sussex - Shoreham PCN' ? '#d58ec6' :
      d === 'West Sussex - South Crawley PCN' ? '#d44b92' :
      '#FFFFFF';
  }

function PCNColor(feature) {
  return {
    fillColor: setPCNColor(feature.properties.PCN),
    color: setPCNColor(feature.properties.PCN),
    weight: 1,
    fillOpacity: .5};
}

// Specify that this code should run once the county data request is complete
$.when(pcn).done(function() {

var map = L.map('mapid')
          .setView([50.8379, -0.7827], 10);

var basemap = L.tileLayer(tileUrl, { attribution })
          .addTo(map);



var pcn_boundary = L.geoJSON(pcn.responseJSON,
      {style: PCNColor})
      .addTo(map)
      .bindPopup(function (layer) {
    return '<Strong>'+ layer.feature.properties.PCN + '</Strong><br><br>Number of practices: ' + layer.feature.properties['Number of practices'] + '<br>Registered population (Dec 2019): ' + layer.feature.properties.Patients + '<br>Proportion aged 65+: ' + layer.feature.properties['Proportion aged 65+'] + ' (' + layer.feature.properties['Patients aged 65+'] +' patients)';});

    var lad_boundary = L.geoJSON(lad.responseJSON,
            {style: LADColor})
            .addTo(map);

for (var i = 0; i < gp_overview.length; i++) {
  	gps = new L.circleMarker([gp_overview[i]['lat'],gp_overview[i]['long']],
        {radius: 6,
         color:'black',
         weight: 1,
         opacity:1,
         fillColor: gp_overview[i]['Colours'],
         fillOpacity:1})
         .addTo(map)
         .bindPopup('<Strong>' + gp_overview[i]['Code'] + ' ' + gp_overview[i]['Name'] + '</Strong><br><br>This practice is part of the ' + gp_overview[i]['PCN'] + '. There are ' + d3.format(',.0f')(gp_overview[i]['Patients']) +' patients registered to this practice.');
  		}

// var show_gps = L.layerGroup([gps]);
//
// var overlayMaps = {
//   "Show GPs": show_gps,
//   "Show District boundaries": lad_boundary};
//
// var baseMaps = {
//   "Show PCN boundaries": pcn_boundary};
//
// L.control.layers(baseMaps, overlayMaps, {
//           collapsed: false,
//           hideSingleBase: true,
//           autoZIndex: false
//         })
//          .addTo(map);

         map
          .fitBounds(lad_boundary.getBounds());

});
