(window.webpackJsonp=window.webpackJsonp||[]).push([[11],{334:function(e,n,a){"use strict";a.r(n);var s=a(1),t=function(e){e.options.__data__block__={mermaid_382ee146:"classDiagram\n      Animal <|-- Duck\n      Animal <|-- Fish\n      Animal <|-- Zebra\n      Animal : +int age\n      Animal : +String gender\n      Animal: +isMammal()\n      Animal: +mate()\n      class Duck{\n          +String beakColor\n          +swim()\n          +quack()\n      }\n      class Fish{\n          -int sizeInFeet\n          -canEat()\n      }\n      class Zebra{\n          +bool is_wild\n          +run()\n      }\n",mermaid_382ee14a:"erDiagram\n    CUSTOMER ||--o{ ORDER : places\n    CUSTOMER {\n        string name\n        string custNumber\n        string sector\n    }\n    ORDER ||--|{ LINE-ITEM : contains\n    ORDER {\n        int orderNumber\n        string deliveryAddress\n    }\n    LINE-ITEM {\n        string productCode\n        int quantity\n        float pricePerUnit\n    }\n",mermaid_382ee14e:"sequenceDiagram\nAlice->John: Hello John, how are you?\nloop every minute\n    John--\x3eAlice: Great!\nend\n",mermaid_382ee165:"sequenceDiagram\nAlice->John: Hello John, how are you?\nloop every minute\n    John--\x3eAlice: Great!\nend\n"}},r=Object(s.a)({},(function(){var e=this,n=e._self._c;return n("ContentSlotsDistributor",{attrs:{"slot-key":e.$parent.slotKey}},[n("h2",{attrs:{id:"部门主数据分发过程"}},[n("a",{staticClass:"header-anchor",attrs:{href:"#部门主数据分发过程"}},[e._v("#")]),e._v(" 部门主数据分发过程")]),e._v(" "),n("div",{staticClass:"language-sequence line-numbers-mode"},[n("pre",{pre:!0,attrs:{class:"language-text"}},[n("code",[e._v("SHR->BPM: 通知BPM部门信息发生变更\nNote right of BPM: BPM自动发起《部门维护流程》,并推送MDM\nBPM->MDM: 1st通知，部门维护流程第一次发起\nBPM->BPM: 2nd补充部门对应的成本中心，自动带出利润中心、公司代码\nBPM->MDM: 3rd再次通知，部门信息手工补全后\n\nMDM->云简: 通知部门数据发生变化\n")])]),e._v(" "),n("div",{staticClass:"line-numbers-wrapper"},[n("span",{staticClass:"line-number"},[e._v("1")]),n("br"),n("span",{staticClass:"line-number"},[e._v("2")]),n("br"),n("span",{staticClass:"line-number"},[e._v("3")]),n("br"),n("span",{staticClass:"line-number"},[e._v("4")]),n("br"),n("span",{staticClass:"line-number"},[e._v("5")]),n("br"),n("span",{staticClass:"line-number"},[e._v("6")]),n("br"),n("span",{staticClass:"line-number"},[e._v("7")]),n("br")])]),n("h2",{attrs:{id:"hr主数据分发过程"}},[n("a",{staticClass:"header-anchor",attrs:{href:"#hr主数据分发过程"}},[e._v("#")]),e._v(" HR主数据分发过程")]),e._v(" "),n("div",{staticClass:"language-sequence line-numbers-mode"},[n("pre",{pre:!0,attrs:{class:"language-text"}},[n("code",[e._v("SHR->SHR: 入职、再入职、离职、退休、退休返聘\nSHR->BPM: 通知BPM人员信息发生变更\nNote right of BPM: BPM自动发起《人员维护流程》,并推送MDM\nBPM->BPM: 1.从BPM维护的人员成本中心映射表<成本中心维护>中读取数据,\\n a.如果读取到信息，以该表数据为准。\\n b.如果未读取到数据，取人员所在部门的成本中心、利润中心\nBPM->MDM: 2.自动提交《人员维护流程》\nMDM->云简: 通知部门数据发生变化\n\n")])]),e._v(" "),n("div",{staticClass:"line-numbers-wrapper"},[n("span",{staticClass:"line-number"},[e._v("1")]),n("br"),n("span",{staticClass:"line-number"},[e._v("2")]),n("br"),n("span",{staticClass:"line-number"},[e._v("3")]),n("br"),n("span",{staticClass:"line-number"},[e._v("4")]),n("br"),n("span",{staticClass:"line-number"},[e._v("5")]),n("br"),n("span",{staticClass:"line-number"},[e._v("6")]),n("br"),n("span",{staticClass:"line-number"},[e._v("7")]),n("br")])]),n("h2",{attrs:{id:"test"}},[n("a",{staticClass:"header-anchor",attrs:{href:"#test"}},[e._v("#")]),e._v(" Test")]),e._v(" "),n("Mermaid",{attrs:{id:"mermaid_382ee146",graph:e.$dataBlock.mermaid_382ee146}}),n("p",[e._v("##Test2")]),e._v(" "),n("Mermaid",{attrs:{id:"mermaid_382ee14a",graph:e.$dataBlock.mermaid_382ee14a}}),n("h2",{attrs:{id:"test3"}},[n("a",{staticClass:"header-anchor",attrs:{href:"#test3"}},[e._v("#")]),e._v(" test3")]),e._v(" "),n("Mermaid",{attrs:{id:"mermaid_382ee14e",graph:e.$dataBlock.mermaid_382ee14e}}),n("mermaid",[e._v("\ngraph lR\nDocumentation--with diagrams--\x3e_[is Awesome]\n")]),e._v(" "),n("Mermaid",{attrs:{id:"mermaid_382ee165",graph:e.$dataBlock.mermaid_382ee165}})],1)}),[],!1,null,null,null);"function"==typeof t&&t(r);n.default=r.exports}}]);