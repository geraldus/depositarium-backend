import React from 'react'


export class CurrenciesTable extends React.Component {
    findDescription (c) {
        const { descriptions } = this.props.items
        // FIXME: Read language somewhere
        const currentLang = "ru"
        return (descriptions.find((e) => {
            return (e.language == currentLang && e.currency == c.id)
        }) || { shortName: '' })

    }
    render () {
        const L = this.props.labels
        const { currencies } = this.props.items

        return (
            <table className="table table-hover">
                <thead>
                    <tr className="d-none d-sm-table-row">
                        <th>{L.general}</th>
                        <th>{L.details}</th>
                        <th>{L.languages}</th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>
                    {currencies.map((c, idx) => {
                        const d = this.findDescription(c)
                        return (<React.Fragment key={idx}>
                            <tr className="d-none d-sm-table-row">
                                <td>
                                    <div>
                                        {d.htmlName}
                                        {d.abbr && ` (${d.abbr})`}
                                    </div>
                                    <div>
                                        <strong className="mr-2">{c.sign}</strong>
                                        <span
                                                className="mx-2"
                                                title={L.code}>
                                            {c.code}
                                        </span>
                                        <span
                                                className="ml-2"
                                                title={L.ident}>
                                            {c.ident}
                                        </span>
                                    </div>
                                    <div>
                                        {c.type == 'CryptoCurrency'
                                            ? L.cryptoCurrency
                                            : L.fiatCurrency}
                                    </div>
                                </td>
                                <td>
                                    {d.htmlLongName == ''
                                        ? <div><i>{L.noDescrition}</i></div>
                                        : <div dangerouslySetInnerHTML={{__html: d.htmlLongName}}></div>
                                    }
                                    {console.log(d.htmlLongDesc, d.htmlLongDesc == '')}
                                    {d.htmlDesc == ''
                                        ? <div><i>{L.noDescrition}</i></div>
                                        : <div dangerouslySetInnerHTML={{__html: d.htmlDesc}}></div>
                                    }
                                </td>
                                <td>
                                    <span className="badge badge-light">RU</span>
                                </td>
                                <td>
                                    <a
                                        href='#'
                                        title={L.edit}>
                                        <i className="fas fa-edit"/>
                                    </a>
                                </td>
                            </tr>
                            <tr className="d-sm-none">
                                <td>
                                    <div className="card">
                                        <h5 className="card-header">
                                            <strong>{c.sign}</strong>
                                            {` `}
                                            {c.code}
                                            <span
                                                    className="badge badge-secondary ml-5">
                                                RU
                                            </span>
                                        </h5>
                                        <div className="card-body">
                                            <h5 className="card-title">
                                                {d.htmlName}{d.abbr && ` (${d.abbr})`}
                                            </h5>
                                            <h6
                                                    className="card-subtitle mb-2 text-muted"
                                                    >
                                                {c.type == 'CryptoCurrency'
                                                ? L.cryptoCurrency
                                                : L.fiatCurrency}</h6>
                                            <p
                                                    className="card-text"
                                                    dangerouslySetInnerHTML={
                                                        {__html: d.htmlLongName}
                                                    }></p>

                                            <a
                                                    href="#"
                                                    title={L.edit}
                                                    className="card-link">
                                                <i className="fas fa-edit"/>
                                                {L.edit}
                                            </a>
                                        </div>
                                    </div>
                                </td>
                            </tr>
                        </React.Fragment>)
                    })}
                </tbody>
            </table>
        )
    }
}

CurrenciesTable.defaultProps = {
    labels: {
        general: 'Основное',
        details: 'Дополнительно',
        sign: 'Знак',
        description: 'Описание',
        code: 'Международный код',
        ident: 'Ярлык',
        cryptoCurrency: 'Криптовалюта',
        fiatCurrency: 'Фиатные деньги',
        shortName: 'Короткое описание',
        longName: 'Длинное описание',
        noDescrition: 'Описание отсутствует',
        abbr: 'Сокращение',
        edit: 'Править',
        languages: 'Языки'
    }
}