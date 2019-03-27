import React from 'react'

import AppNav from './app/nav'


export default class App extends React.Component {
    renderRoute () {
        const l = window.location
        const { nav } = this.props
        const c = nav.routes.find((e) => {
            return e.path === l.pathname
        })
        console.log(l)
        console.log(nav.routes)
        console.log(c)
        if (typeof c !== typeof undefined) {
            return (props) => <c { ... props }/>
        } else {
            return () => null
        }
    }

    render () {
        const SelectedRoute = this.renderRoute()
        return (<React.Fragment>
            <AppNav { ... this.props.nav }/>
            <SelectedRoute/>
        </React.Fragment>)
    }

}