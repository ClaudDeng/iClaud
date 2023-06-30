//  const sidebarConf = require("./config/sidebarConf");
 const pluginsConf = require("./config/pluginsConf");
 const navConf = require("./config/navConf");
module.exports = {
    title: 'Claud',
    description: 'Claud\'s 个人笔记 ' ,
    lastUpdated: '上次更新',
    head: [
        ['link', { rel: 'icon', href: '/favicon.ico' }],
        ['meta', { name: 'author', content: 'Claud' }],
        ['meta', { name: 'keywords', content: 'Claud,ABAP,CDS,SAP,RAP,BAPI,RFC,UI5,Vuepress' }],

      ],
    plugins: pluginsConf,
    themeConfig: {
        nav: navConf

    }
}

