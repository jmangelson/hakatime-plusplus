import m from "mithril";
import _ from "lodash";
import path from "path";
import ApexCharts from "apexcharts/dist/apexcharts.common";
import Litepicker from "litepicker";
//import {EditableTable, IEditableTable} from "mithril-table";
import TextField from "polythene-mithril";

// Models
import TimeRange from "../models/TimeRange.js";
import OverviewState from "../models/State.js";
import LocalState from "../models/ProjectState.js";
import ConfigState from "../models/ConfigState.js";

// Utils
import { mkSingleStatCard } from "../single_stat_card.js";
import cards from "../card_container.js";
import utils from "../utils.js";
import config from "../config.js";
import * as auth from "../auth";

//export interface IClass {class_name: string;}

function class_table() {
    return m("table", [
        m("tr", [
            m("th", "Class Name               "),m("td", "________|")
        ]),
		ConfigState.classes.map(function(item) {
			return m("tr", [
				m("td", item), m("td", " ")
			])
		})
    ]);
};

function mkClassConfigBox() {
    return cards.mkCardContainer(
        "Classes",
        m("div.row", [
            class_table(),
            m("div.col.mr-2", {style: {textAlign: "right", margin: "0px"}}, [
                m("div.row", ["Add New Class"]),
                m("div.row", [
                    m('input',{
                        id: "newClassNameTextbox",
                        type: "text",
                        onchange: newInput => {
                            console.log("New Class Name Input: " + newInput.target.value)
                        }
                    }),
                    m("div.mr-1", [
                        m(
                            "button.btn.btn-primary[title='Add New Class']",
                            {
                                onclick: e => {
                                    let new_class = document.getElementById("newClassNameTextbox").value;
                                    //ConfigState.classes.push( {class_name: String(new_class)} );
                                    //TODO: Request Add Here instead of local change
                                    m.request({
                                        method: "GET",
                                        url: "/api/v1/users/current/config/classes/add/" + String(new_class), 
                                        background: true,
                                        responseType:"json",
                                        headers: {
                                            authorization: auth.getHeaderToken()
                                        }
                                    })
                                        .then(function (r) {
                                            ConfigState.fetchCurrentConfig();
                                        })
                                        .catch(function (e) {
                                            // TODO: Show a visual to the user
                                            console.log(e);
                                        });
                                },
                                role: "button"
                            },
                            "Add"
                        )
                    ]),                   
                ]),
                m("div.row", [" "]),                
                m("div.row", ["Remove Class"]),
                m("div.row", [
                    m("div.dropdown.mr-2", [
                        m(
                            "button.btn.btn-primary.dropdown-toggle.shadow-sm[data-toggle='dropdown'][aria-haspopup='true'][aria-expanded='false']",
                            {
                                type: "button",
                                id: "dropdownMenuButton"
                            },
                            [
                                m("i.fas.fa-clock.fa-md.text-white-50.mr-2"),
                                m("small", `Select Class to Remove`)
                            ]
                        ),
                        m(
                            'div.dropdown-menu[aria-labelledby="dropdownMenuButton"]',
                            ConfigState.classes.map(r => {
                                return m(
                                    "a.btn.dropdown-item",
                                    {
                                        onclick: () => {
                                            //Requrest remove here
                                            m.request({
                                                method: "GET",
                                                url: "/api/v1/users/current/config/classes/remove/" + `${r}`,
                                                background: true,
                                                responseType:"json",
                                                headers: {
                                                    authorization: auth.getHeaderToken()
                                                }
                                            })
                                                .then(function (r) {
                                                    ConfigState.fetchCurrentConfig();
                                                })
                                                .catch(function (e) {
                                                    // TODO: Show a visual to the user
                                                    console.log(e);
                                                });
                                        }
                                    },
                                    `${r}`
                                );
                            })
                        )
                    ])
                ])
            ]),
        ]),
    );
}

function task_state_table() {
    return m("table", [
        m("tr", [
            m("th", "Task State               "), m("td", "_______|")
        ]),
		ConfigState.task_states.map(function(item) {
			return m("tr", [
				m("td", item), m("td", " ")
			])
		})
    ]);
};

function mkTaskStateConfigBox() {
    return cards.mkCardContainer(
        "Possible Task States",
        m("div.row", [
            task_state_table(),
            m("div.col.mr-2", {style: {textAlign: "right", margin: "0px"}}, [
                m("div.row", ["Add New Task State"]),
                m("div.row", [
                    m('input',{
                        id: "newTaskStateTextbox",
                        type: "text",
                        onchange: newInput => {
                            console.log("New Task State Input: " + newInput.target.value)
                        }
                    }),
                    m("div.mr-1", [
                        m(
                            "button.btn.btn-primary[title='Add New State']",
                            {
                                onclick: e => {
                                    let new_state = document.getElementById("newTaskStateTextbox").value;
                                    //ConfigState.classes.push( {class_name: String(new_class)} );
                                    //TODO: Request Add Here instead of local change
                                    m.request({
                                        method: "GET",
                                        url: "/api/v1/users/current/config/taskstate/add/" + String(new_state), 
                                        background: true,
                                        responseType:"json",
                                        headers: {
                                            authorization: auth.getHeaderToken()
                                        }
                                    })
                                        .then(function (r) {
                                            ConfigState.fetchCurrentConfig();
                                        })
                                        .catch(function (e) {
                                            // TODO: Show a visual to the user
                                            console.log(e);
                                        });
                                },
                                role: "button"
                            },
                            "Add"
                        )
                    ]),                   
                ]),
                m("div.row", [" "]),                
                m("div.row", ["Remove State"]),
                m("div.row", [
                    m("div.dropdown.mr-2", [
                        m(
                            "button.btn.btn-primary.dropdown-toggle.shadow-sm[data-toggle='dropdown'][aria-haspopup='true'][aria-expanded='false']",
                            {
                                type: "button",
                                id: "dropdownMenuButton"
                            },
                            [
                                m("i.fas.fa-clock.fa-md.text-white-50.mr-2"),
                                m("small", `Select State to Remove`)
                            ]
                        ),
                        m(
                            'div.dropdown-menu[aria-labelledby="dropdownMenuButton"]',
                            ConfigState.task_states.map(r => {
                                return m(
                                    "a.btn.dropdown-item",
                                    {
                                        onclick: () => {
                                            //Requrest remove here
                                            m.request({
                                                method: "GET",
                                                url: "/api/v1/users/current/config/taskstate/remove/" + `${r}`,
                                                background: true,
                                                responseType:"json",
                                                headers: {
                                                    authorization: auth.getHeaderToken()
                                                }
                                            })
                                                .then(function (r) {
                                                    ConfigState.fetchCurrentConfig();
                                                })
                                                .catch(function (e) {
                                                    // TODO: Show a visual to the user
                                                    console.log(e);
                                                });
                                        }
                                    },
                                    `${r}`
                                );
                            })
                        )
                    ])
                ])
            ]),
        ]),
    );
}

function goal_class_table() {
    return m("table", [
        m("tr", [
            m("th", "Goal Class Name               "), m("td", "______|")
        ]),
		ConfigState.goal_classes.map(function(item) {
			return m("tr", [
				m("td", item), m("td", " ")
			])
		})
    ]);
};

function mkGoalClassConfigBox() {
    return cards.mkCardContainer(
        "Goal Classes",
        m("div.row", [
            goal_class_table(),
            m("div.col.mr-2", {style: {textAlign: "right", margin: "0px"}}, [
                m("div.row", ["Add New Goal Class"]),
                m("div.row", [
                    m('input',{
                        id: "newGoalClassNameTextbox",
                        type: "text",
                        onchange: newInput => {
                            console.log("New Goal Class Name Input: " + newInput.target.value)
                        }
                    }),
                    m("div.mr-1", [
                        m(
                            "button.btn.btn-primary[title='Add New Goal Class']",
                            {
                                onclick: e => {
                                    let new_goal_class = document.getElementById("newGoalClassNameTextbox").value;
                                    //ConfigState.goal_classes.push( {goal_class_name: String(new_goal_class)} );
                                    //TODO: Request Add Here instead of local change
                                    m.request({
                                        method: "GET",
                                        url: "/api/v1/users/current/config/goalclasses/add/" + String(new_goal_class), 
                                        background: true,
                                        responseType:"json",
                                        headers: {
                                            authorization: auth.getHeaderToken()
                                        }
                                    })
                                        .then(function (r) {
                                            ConfigState.fetchCurrentConfig();
                                        })
                                        .catch(function (e) {
                                            // TODO: Show a visual to the user
                                            console.log(e);
                                        });
                                },
                                role: "button"
                            },
                            "Add"
                        )
                    ]),                   
                ]),
                m("div.row", [" "]),                
                m("div.row", ["Remove Goal Class"]),
                m("div.row", [
                    m("div.dropdown.mr-2", [
                        m(
                            "button.btn.btn-primary.dropdown-toggle.shadow-sm[data-toggle='dropdown'][aria-haspopup='true'][aria-expanded='false']",
                            {
                                type: "button",
                                id: "dropdownMenuButton"
                            },
                            [
                                m("i.fas.fa-clock.fa-md.text-white-50.mr-2"),
                                m("small", `Select Goal Class to Remove`)
                            ]
                        ),
                        m(
                            'div.dropdown-menu[aria-labelledby="dropdownMenuButton"]',
                            ConfigState.goal_classes.map(r => {
                                return m(
                                    "a.btn.dropdown-item",
                                    {
                                        onclick: () => {
                                            //Requrest remove here
                                            m.request({
                                                method: "GET",
                                                url: "/api/v1/users/current/config/goalclasses/remove/" + `${r}`,
                                                background: true,
                                                responseType:"json",
                                                headers: {
                                                    authorization: auth.getHeaderToken()
                                                }
                                            })
                                                .then(function (r) {
                                                    ConfigState.fetchCurrentConfig();
                                                })
                                                .catch(function (e) {
                                                    // TODO: Show a visual to the user
                                                    console.log(e);
                                                });
                                        }
                                    },
                                    `${r}`
                                );
                            })
                        )
                    ])
                ])
            ]),
        ]),
    );
}


function major_cat_table() {
    return m("table", [
        m("tr", [
            m("th", "Major Category"), m("td", "_______|")
        ]),
		ConfigState.major_categories.map(function(item) {
			return m("tr", [
				m("td", `${item.mcr_name}`), m("td", " ")
			])
		})
    ]);
};

function mkMajCatConfigBox() {
    return cards.mkCardContainer(
        "Major Categories",
        m("div.row", [
            major_cat_table(),
            m("div.col.mr-2", {style: {textAlign: "right", margin: "0px"}}, [
                m("div.row", ["Add New Major Category"]),
                m("div.row", [
                    m('input',{
                        id: "newMajCatNameTextbox",
                        type: "text",
                        onchange: newInput => {
                            console.log("New Major Category Input: " + newInput.target.value)
                        }
                    }),
                    m("div.mr-1", [
                        m(
                            "button.btn.btn-primary[title='Add New Major Category']",
                            {
                                onclick: e => {
                                    let new_major_cat = document.getElementById("newMajCatNameTextbox").value;
                                    //ConfigState.major_cates.push( {major_cat_name: String(new_major_cat)} );
                                    //TODO: Request Add Here instead of local change
                                    m.request({
                                        method: "GET",
                                        url: "/api/v1/users/current/config/majcat/add/" + String(new_major_cat), 
                                        background: true,
                                        responseType:"json",
                                        headers: {
                                            authorization: auth.getHeaderToken()
                                        }
                                    })
                                        .then(function (r) {
                                            ConfigState.fetchCurrentConfig();
                                        })
                                        .catch(function (e) {
                                            // TODO: Show a visual to the user
                                            console.log(e);
                                        });
                                },
                                role: "button"
                            },
                            "Add"
                        )
                    ]),                   
                ]),
                m("div.row", [" "]),                
                m("div.row", ["Remove Major Category"]),
                m("div.row", [
                    m("div.dropdown.mr-2", [
                        m(
                            "button.btn.btn-primary.dropdown-toggle.shadow-sm[data-toggle='dropdown'][aria-haspopup='true'][aria-expanded='false']",
                            {
                                type: "button",
                                id: "dropdownMenuButton"
                            },
                            [
                                m("i.fas.fa-clock.fa-md.text-white-50.mr-2"),
                                m("small", `Select Category to Remove`)
                            ]
                        ),
                        m(
                            'div.dropdown-menu[aria-labelledby="dropdownMenuButton"]',
                            ConfigState.major_categories.map(r => {
                                return m(
                                    "a.btn.dropdown-item",
                                    {
                                        onclick: () => {
                                            //Requrest remove here
                                            m.request({
                                                method: "GET",
                                                url: "/api/v1/users/current/config/majcat/remove/" + `${r.mcr_name}`,
                                                background: true,
                                                responseType:"json",
                                                headers: {
                                                    authorization: auth.getHeaderToken()
                                                }
                                            })
                                                .then(function (r) {
                                                    ConfigState.fetchCurrentConfig();
                                                })
                                                .catch(function (e) {
                                                    // TODO: Show a visual to the user
                                                    console.log(e);
                                                });
                                        }
                                    },
                                    `${r.mcr_name}`
                                );
                            })
                        )
                    ])
                ])
            ]),
        ]),
    );
}

function sub_cat_table() {
    return m("table", [
        m("tr", [
            m("th", "Sub Category ID"),
            m("th", " | "),
            m("th", "Major Category"),
            m("th", " | "),            
            m("th", "Sub Category Name"),
            m("td", "_______|")
        ]),
		ConfigState.sub_categories.map(function(item) {
            let mcr_name = ConfigState.getMajCatName(item.scr_major_category_id);
			return m("tr", [
				m("td", `${item.scr_id}`),
                m("td", " | "),                
				m("td", mcr_name),
                m("td", " | "),                                
				m("td", `${item.scr_name}`),
                m("td", " ")
			])
		})
    ]);
};

function mkSubCatConfigBox() {
    return cards.mkCardContainer(
        "Sub Categories",
        m("div.row", [
            sub_cat_table(),
            m("div.col.mr-2", {style: {textAlign: "right", margin: "0px"}}, [
                m("div.row", ["Add New Sub Category"]),
                m("div.row", [
                    m('input',{
                        id: "newSubCatNameTextbox",
                        type: "text",
                        onchange: newInput => {
                            console.log("New Sub Category Input: " + newInput.target.value)
                        }
                    }),
                ]),
                m("div.row", [
                    m("div.dropdown.mr-2", [
                        m(
                            "button.btn.btn-primary.dropdown-toggle.shadow-sm[data-toggle='dropdown'][aria-haspopup='true'][aria-expanded='false']",
                            {
                                type: "button",
                                id: "dropdownMenuButton"
                            },
                            [
                                m("i.fas.fa-clock.fa-md.text-white-50.mr-2"),
                                m("small", `Major Category (${ConfigState.selected_maj_cat_name})`)
                            ]
                        ),
                        m(
                            'div.dropdown-menu[aria-labelledby="dropdownMenuButton"]',
                            ConfigState.major_categories.map(r => {
                                return m(
                                    "a.btn.dropdown-item",
                                    {
                                        onclick: () => {
                                            ConfigState.setSelectedMajCat(r.mcr_name, r.mcr_id);
                                        }
                                    },
                                    `${r.mcr_name}`
                                );
                            })
                        )
                    ]),
                ]),
                m("div.row", [
                    m("div.mr-1", [
                        m(
                            "button.btn.btn-primary[title='Add New Sub Category']",
                            {
                                onclick: e => {
                                    let new_sub_cat = document.getElementById("newSubCatNameTextbox").value;
                                    //ConfigState.sub_cates.push( {sub_cat_name: String(new_sub_cat)} );
                                    //TODO: Request Add Here instead of local change
                                    m.request({
                                        method: "GET",
                                        url: "/api/v1/users/current/config/subcat/add/" + String(new_sub_cat) + "/" + String(ConfigState.selected_maj_cat_id), 
                                        background: true,
                                        responseType:"json",
                                        headers: {
                                            authorization: auth.getHeaderToken()
                                        }
                                    })
                                        .then(function (r) {
                                            ConfigState.fetchCurrentConfig();
                                        })
                                        .catch(function (e) {
                                            // TODO: Show a visual to the user
                                            console.log(e);
                                        });
                                },
                                role: "button"
                            },
                            "Add"
                        )
                    ]),                   
                ]),
                m("div.row", [" "]),                
                m("div.row", ["Remove Sub Category"]),
                m("div.row", [
                    m("div.dropdown.mr-2", [
                        m(
                            "button.btn.btn-primary.dropdown-toggle.shadow-sm[data-toggle='dropdown'][aria-haspopup='true'][aria-expanded='false']",
                            {
                                type: "button",
                                id: "dropdownMenuButton"
                            },
                            [
                                m("i.fas.fa-clock.fa-md.text-white-50.mr-2"),
                                m("small", `Select Sub Category to Remove`)
                            ]
                        ),
                        m(
                            'div.dropdown-menu[aria-labelledby="dropdownMenuButton"]',
                            ConfigState.sub_categories.map(r => {
                                return m(
                                    "a.btn.dropdown-item",
                                    {
                                        onclick: () => {
                                            //Requrest remove here
                                            m.request({
                                                method: "GET",
                                                url: "/api/v1/users/current/config/subcat/remove/" + `${r.scr_name}` + `/` + `${r.scr_major_category_id}`,
                                                background: true,
                                                responseType:"json",
                                                headers: {
                                                    authorization: auth.getHeaderToken()
                                                }
                                            })
                                                .then(function (r) {
                                                    ConfigState.fetchCurrentConfig();
                                                })
                                                .catch(function (e) {
                                                    // TODO: Show a visual to the user
                                                    console.log(e);
                                                });
                                        }
                                    },
                                    `${r.scr_id}`
                                );
                            })
                        )
                    ])
                ])
            ]),
        ]),
    );
}


function subsub_cat_table() {
    return m("table", [
        m("tr", [
            m("th", "SubSub Category ID"),
            m("th", " | "),
            m("th", "Major Category"),
            m("th", " | "),
            m("th", "Sub Category"),
            m("th", " | "),                        
            m("th", "Sub-Sub Category Name"),
            m("td", "_______|")
        ]),
		ConfigState.subsub_categories.map(function(item) {
            let mcr_name = ConfigState.getMajCatName(item.sscr_major_category_id);
            let scr_name = ConfigState.getSubCatName(item.sscr_sub_category_id);            
			return m("tr", [
				m("td", `${item.sscr_id}`),
                m("td", " | "),                
				m("td", mcr_name),
                m("td", " | "),
				m("td", scr_name),
                m("td", " | "),                                                
				m("td", `${item.sscr_name}`),
                m("td", " ")
			])
		})
    ]);
};

function mkSubsubCatConfigBox() {
    return cards.mkCardContainer(
        "Sub-Sub Categories",
        m("div.row", [
            subsub_cat_table(),
            m("div.col.mr-2", {style: {textAlign: "right", margin: "0px"}}, [
                m("div.row", ["Add New Sub-Sub Category"]),
                m("div.row", [
                    m('input',{
                        id: "newSubsubCatNameTextbox",
                        type: "text",
                        onchange: newInput => {
                            console.log("New Sub-Sub Category Input: " + newInput.target.value)
                        }
                    }),
                ]),
                m("div.row", [
                    m("div.dropdown.mr-2", [
                        m(
                            "button.btn.btn-primary.dropdown-toggle.shadow-sm[data-toggle='dropdown'][aria-haspopup='true'][aria-expanded='false']",
                            {
                                type: "button",
                                id: "dropdownMenuButton"
                            },
                            [
                                m("i.fas.fa-clock.fa-md.text-white-50.mr-2"),
                                m("small", `Major Category (${ConfigState.selected_maj_cat_name})`)
                            ]
                        ),
                        m(
                            'div.dropdown-menu[aria-labelledby="dropdownMenuButton"]',
                            ConfigState.major_categories.map(r => {
                                return m(
                                    "a.btn.dropdown-item",
                                    {
                                        onclick: () => {
                                            ConfigState.setSelectedMajCat(r.mcr_name, r.mcr_id);
                                        }
                                    },
                                    `${r.mcr_name}`
                                );
                            })
                        )
                    ]),
                ]),
                m("div.row", [
                    m("div.dropdown.mr-2", [
                        m(
                            "button.btn.btn-primary.dropdown-toggle.shadow-sm[data-toggle='dropdown'][aria-haspopup='true'][aria-expanded='false']",
                            {
                                type: "button",
                                id: "dropdownMenuButton"
                            },
                            [
                                m("i.fas.fa-clock.fa-md.text-white-50.mr-2"),
                                m("small", `Sub Category (${ConfigState.selected_sub_cat_name})`)
                            ]
                        ),
                        m(
                            'div.dropdown-menu[aria-labelledby="dropdownMenuButton"]',
                            ConfigState.sub_categories.map(r => {
                                return m(
                                    "a.btn.dropdown-item",
                                    {
                                        onclick: () => {
                                            ConfigState.setSelectedSubCat(r.scr_name, r.scr_id);
                                        }
                                    },
                                    `${r.scr_name}`
                                );
                            })
                        )
                    ]),
                ]),                
                m("div.row", [
                    m("div.mr-1", [
                        m(
                            "button.btn.btn-primary[title='Add New Sub-Sub Category']",
                            {
                                onclick: e => {
                                    let new_subsub_cat = document.getElementById("newSubsubCatNameTextbox").value;
                                    //ConfigState.subsub_cates.push( {subsub_cat_name: String(new_subsub_cat)} );
                                    //TODO: Request Add Here instead of local change
                                    m.request({
                                        method: "GET",
                                        url: "/api/v1/users/current/config/subsubcat/add/" + String(new_subsub_cat) + "/" + String(ConfigState.selected_maj_cat_id) + "/" + String(ConfigState.selected_sub_cat_id), 
                                        background: true,
                                        responseType:"json",
                                        headers: {
                                            authorization: auth.getHeaderToken()
                                        }
                                    })
                                        .then(function (r) {
                                            ConfigState.fetchCurrentConfig();
                                        })
                                        .catch(function (e) {
                                            // TODO: Show a visual to the user
                                            console.log(e);
                                        });
                                },
                                role: "button"
                            },
                            "Add"
                        )
                    ]),                   
                ]),
                m("div.row", [" "]),                
                m("div.row", ["Remove Sub-Sub Category"]),
                m("div.row", [
                    m("div.dropdown.mr-2", [
                        m(
                            "button.btn.btn-primary.dropdown-toggle.shadow-sm[data-toggle='dropdown'][aria-haspopup='true'][aria-expanded='false']",
                            {
                                type: "button",
                                id: "dropdownMenuButton"
                            },
                            [
                                m("i.fas.fa-clock.fa-md.text-white-50.mr-2"),
                                m("small", `Select Sub-Sub Category to Remove`)
                            ]
                        ),
                        m(
                            'div.dropdown-menu[aria-labelledby="dropdownMenuButton"]',
                            ConfigState.subsub_categories.map(r => {
                                return m(
                                    "a.btn.dropdown-item",
                                    {
                                        onclick: () => {
                                            //Requrest remove here
                                            m.request({
                                                method: "GET",
                                                url: "/api/v1/users/current/config/subsubcat/remove/" + `${r.sscr_name}` + `/` + `${r.sscr_major_category_id}` + `/` + `${r.sscr_sub_category_id}`,
                                                background: true,
                                                responseType:"json",
                                                headers: {
                                                    authorization: auth.getHeaderToken()
                                                }
                                            })
                                                .then(function (r) {
                                                    ConfigState.fetchCurrentConfig();
                                                })
                                                .catch(function (e) {
                                                    // TODO: Show a visual to the user
                                                    console.log(e);
                                                });
                                        }
                                    },
                                    `${r.sscr_id}`
                                );
                            })
                        )
                    ])
                ])
            ]),
        ]),
    );
}

// function mkFileChart() {m
//   return cards.mkCardContainer("Most active files", m(fileChart()));
// }

// function mkLineChart() {
//   return cards.mkCardContainer("Total activity", m(barChart()));
// }

// function mkPieChart() {
//   return cards.mkCardContainer("Language breakdown", m(pieChart()));
// }

// function mkWeekDayRadar() {
//   return cards.mkCardContainer("Activity per weekday", m(dayRadarChart()));
// }

// function mkHourDistribution() {
//   return cards.mkCardContainer(
//     "Activity per hour of day",
//     m(hourDistribution())
//   );
// }

// function hourDistribution() {
//   let _chart = null;

//   return {
//     view: () => {
//       return m("div.chart");
//     },

//     onbeforeremove: () => {
//       if (_chart) {
//         _chart.destroy();
//         _chart = null;
//       }
//     },

//     oncreate: vnode => {
//       if (LocalState.obj == null || LocalState.obj.totalSeconds === 0) return;

//       const data = _.zipObject([...Array(24).keys()], Array(24).fill(0));
//       _.forEach(LocalState.obj.hour, function (v) {
//         data[utils.addTimeOffset(v.name)] = (v.totalSeconds / 3600).toFixed(1);
//       });

//       const options = {
//         series: [
//           {
//             data: _.toArray(data),
//             name: "Activity"
//           }
//         ],
//         noData: config.noData,
//         tooltip: {
//           y: {
//             formatter: function (val) {
//               return val + " hours";
//             }
//           }
//         },
//         dataLabels: {
//           enabled: false
//         },
//         chart: {
//           type: "bar",
//           height: "300",
//           toolbar: config.toolbar,
//           animations: config.animations
//         },
//         plotOptions: {
//           bar: {
//             columnWidth: "40%",
//             endingShape: "rounded"
//           }
//         },
//         yaxis: {
//           title: {
//             text: "Hours"
//           }
//         },
//         xaxis: {
//           categories: [...Array(24).keys()]
//         }
//       };

//       _chart = new ApexCharts(vnode.dom, options);
//       _chart.render();
//     }
//   };
// }

// function dayRadarChart() {
//   let _chart = null;

//   return { 
//     view: () => {
//       return m("div.chart");
//     },

//     onbeforeremove: () => {
//       if (_chart) {
//         _chart.destroy();
//         _chart = null;
//       }
//     },

//     oncreate: vnode => {
//       if (LocalState.obj == null) return;

//       const data = _.zipObject([...Array(7).keys()], Array(7).fill(0));
//       _.forEach(LocalState.obj.weekDay, function (v) {
//         data[v.name] = (v.totalSeconds / 3600).toFixed(1);
//       });

//       const options = {
//         series: [
//           {
//             data: _.toArray(data),
//             name: "Activity"
//           }
//         ],
//         noData: config.noData,
//         tooltip: {
//           y: {
//             formatter: function (val) {
//               return val + " hours";
//             }
//           }
//         },
//         dataLabels: {
//           enabled: false
//         },
//         chart: {
//           type: "radar",
//           height: "300",
//           toolbar: config.toolbar,
//           animations: config.animations
//         },
//         plotOptions: {
//           radar: {
//             size: 120,
//             polygons: {
//               strokeColors: "#e9e9e9",
//               fill: {
//                 colors: ["#f8f8f8", "#fff"]
//               }
//             }
//           }
//         },
//         yaxis: {
//           show: false
//         },
//         xaxis: {
//           categories: [
//             "Sunday",
//             "Monday",
//             "Tuesday",
//             "Wednesday",
//             "Thursday",
//             "Friday",
//             "Saturday"
//           ]
//         }
//       };

//       _chart = new ApexCharts(vnode.dom, options);
//       _chart.render();
//     }
//   };
// }

// function pieChart() {
//   let _chart = null;
//   return {
//     view: () => {
//       return m("div.chart");
//     },

//     onbeforeremove: () => {
//       if (_chart) {
//         _chart.destroy();
//         _chart = null;
//       }
//     },

//     oncreate: vnode => {
//       if (LocalState.obj == null) return;

//       const dataValues = LocalState.obj.languages.map(v => {
//         return {
//           data: parseFloat((v.totalPct * 100).toFixed(2)),
//           name: v.name
//         };
//       });

//       const data = dataValues.map(v => v.data);
//       const names = dataValues.map(v => v.name);

//       const options = {
//         series: data,
//         noData: config.noData,
//         chart: {
//           type: "donut",
//           height: "260",
//           animations: config.animations
//         },
//         labels: names
//       };

//       _chart = new ApexCharts(vnode.dom, options);
//       _chart.render();
//     }
//   };
// }

// /*
//  * Row with single stats only. Each stat has a name, value, and an icon.
//  */
// function mkTopStatRow() {
//   const totalHrs = utils.secondsToHms(utils.getTotalCodingTime(LocalState.obj));

//   return [
//     {
//       name: "Total coding time",
//       value: totalHrs ? `${totalHrs}` : "0",
//       icon: "globe",
//       textType: "primary"
//     },
//     {
//       name: "Languages",
//       value: LocalState.obj.languages.length,
//       icon: "code",
//       textType: "info"
//     },
//     {
//       name: "Files touched",
//       value: LocalState.obj.files.length,
//       icon: "file",
//       textType: "success"
//     },
//     {
//       name: "Most active language",
//       value: utils.getMostActiveLanguage(LocalState.obj),
//       icon: "code",
//       textType: "success"
//     }
//   ].map(conf => {
//     return m("div.col-xl-3.col-md-6.mb-4", m(mkSingleStatCard(conf)));
//   });
// }

// function fileChart() {
//   let _chart = null;
//   return {
//     view: () => {
//       return m("div.chart");
//     },

//     onbeforeremove: () => {
//       if (_chart) {
//         _chart.destroy();
//         _chart = null;
//       }
//     },

//     oncreate: vnode => {
//       if (LocalState.obj == null) return;

//       const myData = _.take(
//         _.orderBy(LocalState.obj.files, ["totalSeconds"], ["desc"]).filter(
//           v => (v.totalSeconds / 3600).toFixed(1) > 0
//         ),
//         10
//       );
//       const data = myData.map(v => (v.totalSeconds / 3600).toFixed(1));
//       const categories = myData.map(v => v.name);

//       const options = {
//         series: [
//           {
//             data: data,
//             name: "Activity"
//           }
//         ],
//         noData: config.noData,
//         tooltip: {
//           y: {
//             formatter: function (val) {
//               return val + " hours";
//             }
//           }
//         },
//         chart: {
//           type: "bar",
//           height: 360,
//           toolbar: config.toolbar,
//           animations: config.animations
//         },
//         plotOptions: {
//           bar: {
//             horizontal: true,
//             distributed: true,
//             columnWidth: "40%",
//             barHeight: "80%",
//             endingShape: "rounded",
//             backgroundBarColors: ["#f8f8f8", "#fff"]
//           }
//         },
//         dataLabels: {
//           enabled: true,
//           textAnchor: "start",
//           style: {
//             colors: ["#fff"],
//             fontFamily: "Nunito"
//           },
//           formatter: function (val, opt) {
//             val = opt.w.globals.labels[opt.dataPointIndex];

//             if (typeof val === "string") {
//               const basename = path.dirname(val).split(path.sep).pop();
//               const filename = val.replace(/^.*[/]/, "");

//               if (!basename) return filename;

//               return path.join(basename, filename);
//             }

//             return val;
//           },
//           offsetX: 0,
//           dropShadow: {
//             enabled: true
//           }
//         },
//         yaxis: {
//           show: false
//         },
//         legend: {
//           show: false
//         },
//         xaxis: {
//           title: {
//             text: "Hours"
//           },
//           categories: categories
//         }
//       };

//       _chart = new ApexCharts(vnode.dom, options);
//       _chart.render();
//     }
//   };
// }

// function barChart() {
//   let _chart = null;

//   return {
//     view: () => {
//       return m("div.chart");
//     },

//     onbeforeremove: () => {
//       if (_chart) {
//         _chart.destroy();
//         _chart = null;
//       }
//     },

//     oncreate: vnode => {
//       if (!LocalState.obj || LocalState.obj.totalSeconds === 0) return;

//       const values = LocalState.obj.dailyTotal.map(v => (v / 3600).toFixed(1));

//       const data = _.zip(LocalState.dates, values).map(data => {
//         return { x: data[0], y: data[1] };
//       });

//       const options = {
//         chart: {
//           type: "bar",
//           height: "250",
//           toolbar: config.toolbar,
//           animations: config.animations
//         },
//         series: [
//           {
//             name: LocalState.currentProject,
//             data: data
//           }
//         ],
//         noData: config.noData,
//         xaxis: {
//           type: "datetime"
//         },
//         yaxis: {
//           title: {
//             text: "Hours"
//           }
//         },
//         tooltip: {
//           y: {
//             formatter: function (val) {
//               return val + " hours";
//             }
//           }
//         },
//         dataLabels: {
//           enabled: false
//         },
//         plotOptions: {
//           bar: {
//             columnWidth: "40%",
//             endingShape: "rounded"
//           }
//         }
//       };

//       _chart = new ApexCharts(vnode.dom, options);
//       _chart.render();
//     }
//   };
// }

// let dateRangePicker;

export default {
    oninit: function () {
        if (ConfigState.initialized == false ) {
            ConfigState.initConfig();
            return;
        }
        
    // if (!OverviewState.obj) {
    //   OverviewState.fetchItems(() => {
    //     LocalState.initProjectList(OverviewState.obj.projects);
    //   });

    //   return;
    // }

    // LocalState.initProjectList(OverviewState.obj.projects);
  },
  onremove: () => {
    // if (dateRangePicker) {
    //   dateRangePicker.destroy();
    //   dateRangePicker = null;
    // }
  },
  view: () => {
    document.title = "Hakatime++ | Config";

    if (ConfigState.feteched == false) {
      return m("div.spinner", [
        m("div.bounce1"),
        m("div.bounce2"),
        m("div.bounce3")
      ]);
    }

    // const toolbar = m("div.d-sm-flex.mb-4", [
    //   m(
    //     "h1.h3.mb-0.mr-auto.text-gray-800", "Config"
    //     // LocalState.currentProject ? LocalState.currentProject : "Projects"
    //   ),
    //   m("div.dropdown.mr-2", [
    //     m(
    //       "button.btn.btn-primary.dropdown-toggle.shadow-sm[data-toggle='dropdown'][aria-haspopup='true'][aria-expanded='false']",
    //       {
    //         type: "button",
    //         id: "dropdownMenuButton"
    //       },
    //       [m("i.fas.fa-book.fa-md.text-white-50.mr-2"), m("small", "Projects")]
    //     ),
    //     m(
    //       'div.dropdown-menu[aria-labelledby="dropdownMenuButton"]',
    //       LocalState.projects.map(project => {
    //         return m(
    //           "a.btn.dropdown-item",
    //           {
    //             onclick: LocalState.fetchProjectStats
    //           },
    //           project
    //         );
    //       })
    //     )
    //   ]),
    //   m("div.dropdown.mr-2", [
    //     m(
    //       "button.btn.btn-primary.dropdown-toggle.shadow-sm[data-toggle='dropdown'][aria-haspopup='true'][aria-expanded='false']",
    //       {
    //         type: "button",
    //         id: "dropdownMenuButton"
    //       },
    //       [
    //         m("i.fas.fa-clock.fa-md.text-white-50.mr-2"),
    //         m("small", `Cut-off limit (${TimeRange.timeLimit} mins)`)
    //       ]
    //     ),
    //     m(
    //       'div.dropdown-menu[aria-labelledby="dropdownMenuButton"]',
    //       [5, 10, 15, 20, 30].map(r => {
    //         return m(
    //           "a.btn.dropdown-item",
    //           {
    //             onclick: () => {
    //               if (TimeRange.setTimeLimit(r)) LocalState.fetchProjectStats();
    //             }
    //           },
    //           `${r} mins`
    //         );
    //       })
    //     )
    //   ]),
    //   m("div.dropdown.mr-2", [
    //     m(
    //       "button.btn.btn-primary.dropdown-toggle.shadow-sm[data-toggle='dropdown'][aria-haspopup='true'][aria-expanded='false']",
    //       {
    //         type: "button",
    //         id: "dropdownMenuButton"
    //       },
    //       [
    //         m("i.fas.fa-calendar.fa-md.text-white-50.mr-2"),
    //         m("small", `Date range (${TimeRange.dateRange()} days)`),
    //         m("a", { id: "date-range-picker-project" })
    //       ]
    //     ),
    //     m(
    //       'div.dropdown-menu[aria-labelledby="dropdownMenuButton"]',
    //       config.dateRangePresets
    //         .map(r => {
    //           return m(
    //             "a.btn.dropdown-item",
    //             {
    //               onclick: () => {
    //                 if (TimeRange.setDaysFromToday(r))
    //                   LocalState.fetchProjectStats();
    //               }
    //             },
    //             `Last ${r} days`
    //           );
    //         })
    //         .concat([
    //           m("div.dropdown-divider"),
    //           m(
    //             "a.btn.dropdown-item",
    //             {
    //               onclick: e => {
    //                 e.redraw = false;

    //                 if (!dateRangePicker) {
    //                   dateRangePicker = new Litepicker({
    //                     ...config.datePicker,
    //                     element: document.getElementById(
    //                       "date-range-picker-project"
    //                     ),
    //                     onSelect: (d1, d2) => {
    //                       if (TimeRange.setDays(d1, d2)) {
    //                         LocalState.fetchProjectStats(
    //                           null,
    //                           d1.toISOString(),
    //                           d2.toISOString()
    //                         );
    //                       }
    //                     }
    //                   });
    //                 }

    //                 dateRangePicker.show();
    //               }
    //             },
    //             "Pick a date range"
    //           )
    //         ])
    //     )
    //   ]),
    //   m("div.mr-1", [
    //     m(
    //       "button.btn.btn-primary[title='Copy shields.io badge to clipboard']",
    //       {
    //         onclick: e => {
    //           e.redraw = false;

    //           m.request({
    //             method: "GET",
    //             url: `/badge/link/${LocalState.currentProject}`,
    //             background: true,
    //             headers: {
    //               authorization: auth.getHeaderToken()
    //             }
    //           })
    //             .then(function (r) {
    //               utils.copyToCliboard(r.badgeUrl);
    //             })
    //             .catch(function (e) {
    //               // TODO: Show a visual to the user
    //               console.log(e);
    //             });
    //         },
    //         role: "button"
    //       },
    //       m("i.fas.fa-clone.fa-md.text-white-50")
    //     )
    //   ])
    // ]);

    return [
      m("div.d-sm-flex.mb-4", [
          m("h1.h3.mb-0.mr-auto.text-gray-800", "Task Configuration Setup")]),
        // LocalState.currentProject ? LocalState.currentProject : "Projects"
        //      )
        //      toolbar,n
        //        m("div.row", mkTopStatRow()),
      m("div.row", [
          m("div.col-xl-6", mkClassConfigBox()),
          m("div.col-xl-6", mkTaskStateConfigBox()),
      ]),
        m("div.d-sm-flex.mb-4", [ m("h1.h3.mb-0.mr-auto.text-gray-800", "Category Configuration Setup")]),
        m("div.col-xl-12",[         
            m("div.row", [ mkMajCatConfigBox() ]),
            m("div.row", [ mkSubCatConfigBox() ]),
            m("div.row", [ mkSubsubCatConfigBox() ]),
        ]),
      m("div.d-sm-flex.mb-4", [
          m("h1.h3.mb-0.mr-auto.text-gray-800", "Goal Configuration Setup")]),
      m("div.row", [
         m("div.col-xl-6", mkGoalClassConfigBox())
      ]),        
      // m("div.row", [
      //   m("div.col-xl-6", mkWeekDayRadar()),
      //   m("div.col-xl-6", mkHourDistribution())
      // ]),
      // m("div.row", [m("div.col-xl-12", mkFileChart())])
    ];
  }
};
