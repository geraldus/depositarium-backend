export const collapsedDetails = (title, data) => {
    console.groupCollapsed(title)
    console.log(data)
    console.groupEnd()
}