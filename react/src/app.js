import React from 'react'

import AppNav from './app/nav'


export default class App extends React.Component {
    currentKnownRoute () {
        const l = window.location
        const { href, pathname } = l
        const { nav } = this.props
        const route = nav.routes.find((e) => e.path === href)
        if (typeof route !== typeof undefined) {
            let r = {  ... route, path: pathname }
            return r
        } else {
            return null
        }
    }
    currentRouteComponent () {
        const route = this.currentKnownRoute ()
        if (route !== null) {
            console.log(route)
            return (props) => <route.component { ... route.props }/>
        } else {
            return () => null
        }
    }

    render () {
        const SelectedComponent = this.currentRouteComponent()
        return (<React.Fragment>
            <AppNav { ... this.props.nav }/>
            <div className="container-fluid">
                <SelectedComponent/>
            </div>
        </React.Fragment>)
    }

}