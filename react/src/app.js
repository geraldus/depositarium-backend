import React from 'react'

import AppNav from './app/nav'


export default (props) => {
    return (<React.Fragment>
        <AppNav { ... props.nav }/>
    </React.Fragment>)
}