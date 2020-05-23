import m from "mithril";
import _ from "lodash";
import path from "path";
import ApexCharts from "apexcharts/dist/apexcharts.common";

// Models
import TimeRange from "../models/TimeRange.js";
import OverviewState from "../models/State.js";
import LocalState from "../models/ProjectState.js";

// Utils
import { mkSingleStatCard } from "../single_stat_card.js";
import cards from "../card_container.js";
import utils from "../utils.js";
import config from "../config.js";

function mkFileChart() {
  return cards.mkCardContainer("Most active files", m(fileChart()));
}

function mkLineChart() {
  return cards.mkCardContainer("Total activity", m(barChart()));
}

function mkPieChart() {
  return cards.mkCardContainer("Language breakdown", m(pieChart()));
}

function mkWeekDayRadar() {
  return cards.mkCardContainer("Activity per weekday", m(dayRadarChart()));
}

function mkHourDistribution() {
  return cards.mkCardContainer(
    "Activity per hour of day",
    m(hourDistribution())
  );
}

function hourDistribution() {
  return {
    view: () => {
      return m("div.chart");
    },
    oncreate: vnode => {
      if (LocalState.obj == null || LocalState.obj.totalSeconds === 0) return;

      const data = _.zipObject([...Array(24).keys()], Array(24).fill(0));
      _.forEach(LocalState.obj.hour, function(v) {
        data[utils.addTimeOffset(v.name)] = (v.totalSeconds / 3600).toFixed(1);
      });

      const options = {
        series: [
          {
            data: _.toArray(data),
            name: "Activity"
          }
        ],
        noData: config.noData,
        tooltip: {
          y: {
            formatter: function(val) {
              return val + " hours";
            }
          }
        },
        dataLabels: {
          enabled: false
        },
        chart: {
          type: "bar",
          height: "300",
          toolbar: config.toolbar
        },
        plotOptions: {
          bar: {
            columnWidth: "40%",
            endingShape: "rounded"
          }
        },
        yaxis: {
          title: {
            text: "Hours"
          }
        },
        xaxis: {
          categories: [...Array(24).keys()]
        }
      };

      const chart = new ApexCharts(vnode.dom, options);
      chart.render();
    }
  };
}

function dayRadarChart() {
  return {
    view: () => {
      return m("div.chart");
    },
    oncreate: vnode => {
      if (LocalState.obj == null) return;

      const data = _.zipObject([...Array(7).keys()], Array(7).fill(0));
      _.forEach(LocalState.obj.weekDay, function(v) {
        data[v.name] = (v.totalSeconds / 3600).toFixed(1);
      });

      const options = {
        series: [
          {
            data: _.toArray(data),
            name: "Activity"
          }
        ],
        noData: config.noData,
        tooltip: {
          y: {
            formatter: function(val) {
              return val + " hours";
            }
          }
        },
        dataLabels: {
          enabled: false
        },
        chart: {
          type: "radar",
          height: "300",
          toolbar: config.toolbar
        },
        plotOptions: {
          radar: {
            size: 120,
            polygons: {
              strokeColors: "#e9e9e9",
              fill: {
                colors: ["#f8f8f8", "#fff"]
              }
            }
          }
        },
        yaxis: {
          show: false
        },
        xaxis: {
          categories: [
            "Sunday",
            "Monday",
            "Tuesday",
            "Wednesday",
            "Thursday",
            "Friday",
            "Saturday"
          ]
        }
      };

      const chart = new ApexCharts(vnode.dom, options);
      chart.render();
    }
  };
}

function pieChart() {
  return {
    view: () => {
      return m("div.chart");
    },

    oncreate: vnode => {
      if (LocalState.obj == null) return;

      const dataValues = LocalState.obj.languages.map(v => {
        return {
          data: parseFloat((v.totalPct * 100).toFixed(2)),
          name: v.name
        };
      });

      const data = dataValues.map(v => v.data);
      const names = dataValues.map(v => v.name);

      const options = {
        series: data,
        noData: config.noData,
        chart: {
          type: "donut",
          height: "260"
        },
        labels: names
      };

      const chart = new ApexCharts(vnode.dom, options);
      chart.render();
    }
  };
}

/*
 * Row with single stats only. Each stat has a name, value, and an icon.
 */
function mkTopStatRow() {
  const totalHrs = utils.secondsToHms(utils.getTotalCodingTime(LocalState.obj));

  return [
    {
      name: "Total coding time",
      value: totalHrs ? `${totalHrs}` : "0",
      icon: "globe",
      textType: "primary"
    },
    {
      name: "Languages",
      value: LocalState.obj.languages.length,
      icon: "code",
      textType: "info"
    },
    {
      name: "Files touched",
      value: LocalState.obj.files.length,
      icon: "file",
      textType: "success"
    },
    {
      name: "Most active language",
      value: utils.getMostActiveLanguage(LocalState.obj),
      icon: "code",
      textType: "success"
    }
  ].map(conf => {
    return m("div.col-xl-3.col-md-6.mb-4", m(mkSingleStatCard(conf)));
  });
}

function fileChart() {
  return {
    view: () => {
      return m("div.chart");
    },

    oncreate: vnode => {
      if (LocalState.obj == null) return;

      const myData = _.take(
        _.orderBy(LocalState.obj.files, ["totalSeconds"], ["desc"]).filter(
          v => (v.totalSeconds / 3600).toFixed(1) > 0
        ),
        10
      );
      const data = myData.map(v => (v.totalSeconds / 3600).toFixed(1));
      const categories = myData.map(v => v.name);

      const options = {
        series: [
          {
            data: data,
            name: "Activity"
          }
        ],
        noData: config.noData,
        tooltip: {
          y: {
            formatter: function(val) {
              return val + " hours";
            }
          }
        },
        chart: {
          type: "bar",
          height: 360,
          toolbar: config.toolbar
        },
        plotOptions: {
          bar: {
            horizontal: true,
            distributed: true,
            columnWidth: "40%",
            barHeight: "80%",
            endingShape: "rounded",
            backgroundBarColors: ["#f8f8f8", "#fff"]
          }
        },
        dataLabels: {
          enabled: true,
          textAnchor: "start",
          style: {
            colors: ["#fff"],
            fontFamily: "Nunito"
          },
          formatter: function(val, opt) {
            val = opt.w.globals.labels[opt.dataPointIndex];

            if (typeof val === "string") {
              const basename = path
                .dirname(val)
                .split(path.sep)
                .pop();
              const filename = val.replace(/^.*[/]/, "");

              if (!basename) return filename;

              return path.join(basename, filename);
            }

            return val;
          },
          offsetX: 0,
          dropShadow: {
            enabled: true
          }
        },
        yaxis: {
          show: false
        },
        legend: {
          show: false
        },
        xaxis: {
          title: {
            text: "Hours"
          },
          categories: categories
        }
      };

      const myChart = new ApexCharts(vnode.dom, options);
      myChart.render();
    }
  };
}

function barChart() {
  return {
    view: () => {
      return m("div.chart");
    },

    oncreate: vnode => {
      if (!LocalState.obj || LocalState.obj.totalSeconds === 0) return;

      const values = LocalState.obj.dailyTotal.map(v => (v / 3600).toFixed(1));

      const data = _.zip(LocalState.dates, values).map(data => {
        return { x: data[0], y: data[1] };
      });

      const options = {
        chart: {
          type: "bar",
          height: "250",
          toolbar: config.toolbar
        },
        series: [
          {
            name: LocalState.currentProject,
            data: data
          }
        ],
        noData: config.noData,
        xaxis: {
          type: "datetime"
        },
        yaxis: {
          title: {
            text: "Hours"
          }
        },
        tooltip: {
          y: {
            formatter: function(val) {
              return val + " hours";
            }
          }
        },
        dataLabels: {
          enabled: false
        },
        plotOptions: {
          bar: {
            columnWidth: "40%",
            endingShape: "rounded"
          }
        }
      };

      const myChart = new ApexCharts(vnode.dom, options);
      myChart.render();
    }
  };
}

export default {
  oninit: function() {
    if (!OverviewState.obj) return;

    LocalState.initProjectList(OverviewState.obj.projects);
  },
  view: () => {
    document.title = "Hakatime | Projects";

    const ranges = [7, 15, 30, 45, 90];
    const toolbar = m("div.d-sm-flex.mb-4", [
      m(
        "h1.h3.mb-0.mr-auto.text-gray-800",
        LocalState.currentProject ? LocalState.currentProject : "Projects"
      ),
      m("div.dropdown.mr-2", [
        m(
          "button.btn.btn-primary.dropdown-toggle.shadow-sm[data-toggle='dropdown'][aria-haspopup='true'][aria-expanded='false']",
          {
            type: "button",
            id: "dropdownMenuButton"
          },
          [m("i.fas.fa-book.fa-md.text-white-50.mr-2"), m("small", "Projects")]
        ),
        m(
          'div.dropdown-menu[aria-labelledby="dropdownMenuButton"]',
          LocalState.projects.map(project => {
            return m(
              "a.btn.dropdown-item",
              {
                onclick: LocalState.fetchProjectStats
              },
              project
            );
          })
        )
      ]),
      m("div.dropdown", [
        m(
          "button.btn.btn-primary.dropdown-toggle.shadow-sm[data-toggle='dropdown'][aria-haspopup='true'][aria-expanded='false']",
          {
            type: "button",
            id: "dropdownMenuButton"
          },
          [
            m("i.fas.fa-calendar.fa-md.text-white-50.mr-2"),
            m("small", `Time range (${TimeRange.numOfDays} days)`)
          ]
        ),
        m(
          'div.dropdown-menu[aria-labelledby="dropdownMenuButton"]',
          ranges.map(r => {
            return m(
              "a.btn.dropdown-item",
              {
                onclick: () => {
                  if (TimeRange.setDays(r)) LocalState.fetchProjectStats();
                }
              },
              `Last ${r} days`
            );
          })
        )
      ])
    ]);

    if (LocalState.obj == null) {
      return m("div.spinner", [
        m("div.bounce1"),
        m("div.bounce2"),
        m("div.bounce3")
      ]);
    }

    return [
      toolbar,
      m("div.row", mkTopStatRow()),
      m("div.row", [
        m("div.col-xl-8.col-lg-7", mkLineChart()),
        m("div.col-xl-4.col-lg-5", mkPieChart())
      ]),
      m("div.row", [
        m("div.col-xl-6", mkWeekDayRadar()),
        m("div.col-xl-6", mkHourDistribution())
      ]),
      m("div.row", [m("div.col-xl-12", mkFileChart())])
    ];
  }
};
